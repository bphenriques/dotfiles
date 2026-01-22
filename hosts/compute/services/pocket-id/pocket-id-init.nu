#!/usr/bin/env nu

# Initializes users and groups. New users are invited.
#
# Limitation: renaming users will create a new one.
const PAGINATION_LIMIT = 20

let base_url = $env.POCKET_ID_URL
let api_key = open $env.POCKET_ID_API_KEY_FILE | str trim
let config = open $env.POCKET_ID_CONFIG_FILE | from json

def api [method: string, endpoint: string, body?: record] {
  let url = $"($base_url)/api($endpoint)"
  let headers = { "X-API-KEY": $api_key, "Content-Type": "application/json" }
  if $body != null {
    http $method $url $body --headers $headers --full --allow-errors
  } else {
    http $method $url --headers $headers --full --allow-errors
  }
}

def wait_ready [] {
  print "Waiting for Pocket ID to be ready..."
  for _ in 1..30 {
    try {
      let r = http get $"($base_url)/.well-known/openid-configuration" --max-time 2 --full --allow-errors
      if $r.status == 200 { print "Pocket ID is ready"; return }
    } catch { }
    sleep 1sec
  }
  error make { msg: "Pocket ID failed to start in time" }
}

def get_all [resource: string] {
  let r = api "get" $"/($resource)?pagination[limit]=($PAGINATION_LIMIT)"
  if $r.status != 200 { error make { msg: $"Failed to list ($resource): ($r.status) - ($r.body)" } }
  $r.body.data
}

def ensure_group [group: record, existing: list] {
  let found = $existing | where name == $group.name | first --skip-empty
  if $found != null { return $found.id }

  let r = api "post" "/user-groups" { name: $group.name, customClaims: [{ key: "managed-by", value: "nix" }] }
  if $r.status != 201 { error make { msg: $"Failed to create group ($group.name): ($r.status) - ($r.body)" } }
  print $"Created group: ($group.name)"
  $r.body.id
}

def ensure_user [user: record, group_ids: list<string>, existing: list] {
  let found = $existing | where username == $user.username | first --skip-empty
  let body = {
    username: $user.username
    email: $user.email
    firstName: $user.firstName
    lastName: $user.lastName
    isAdmin: $user.isAdmin
    customClaims: [{ key: "managed-by", value: "nix" }]
  }

  let user_id = if $found != null {
    let r = api "put" $"/users/($found.id)" $body
    if $r.status != 200 { error make { msg: $"Failed to update user ($user.username): ($r.status) - ($r.body)" } }
    $found.id
  } else {
    let r = api "post" "/users" $body
    if $r.status != 201 { error make { msg: $"Failed to create user ($user.username): ($r.status) - ($r.body)" } }
    print $"Created user: ($user.username)"
    # Send invite for new users
    let ir = api "post" $"/users/($r.body.id)/one-time-access-email" {}
    if $ir.status == 204 { print $"Sent invite email to ($user.email)" }
    $r.body.id
  }

  let gr = api "put" $"/users/($user_id)/user-groups" { userGroupIds: $group_ids }
  if $gr.status != 200 { error make { msg: $"Failed to set groups for ($user.username): ($gr.status) - ($gr.body)" } }
  $user_id
}

def main [] {
  wait_ready

  let existing_groups = get_all "user-groups"
  let group_map = $config.groups 
    | each { |g| { name: $g.name, id: (ensure_group $g $existing_groups) } } 
    | transpose -rd

  let existing_users = get_all "users"
  $config.users | each { |u|
    let group_ids = $u.groups | each { |g| $group_map | get $g }
    ensure_user $u $group_ids $existing_users
  } | ignore

  print "Pocket ID initialization complete"
}
