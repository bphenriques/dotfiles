#!/usr/bin/env nu

let base_url = $env.POCKET_ID_URL
let api_key = open $env.POCKET_ID_API_KEY_FILE | str trim
let groups = $env.POCKET_ID_GROUPS_JSON | from json
let users = $env.POCKET_ID_USERS_JSON | from json

def api [method: string, endpoint: string, body?: record] {
  let url = $"($base_url)/api($endpoint)"
  let headers = { "X-API-KEY": $api_key, "Content-Type": "application/json" }
  if $body != null {
    http $method $url --headers $headers --body $body --full --allow-errors
  } else {
    http $method $url --headers $headers --full --allow-errors
  }
}

def wait_ready [] {
  for i in 1..30 {
    try { http get $base_url --max-time 2 | ignore; return } catch { sleep 1sec }
  }
  error make { msg: "Pocket ID failed to start" }
}

def get_all_groups [] {
  let r = api "get" "/user-groups?pagination[limit]=100"
  if $r.status != 200 { error make { msg: $"Failed to list groups: ($r.status)" } }
  $r.body.data
}

def get_all_users [] {
  let r = api "get" "/users?pagination[limit]=100"
  if $r.status != 200 { error make { msg: $"Failed to list users: ($r.status)" } }
  $r.body.data
}

def ensure_group [name: string] {
  let existing = get_all_groups | where name == $name | first --skip-empty
  if $existing != null { return $existing.id }
  
  let r = api "post" "/user-groups" { name: $name, customClaims: [] }
  if $r.status == 201 { return $r.body.id }
  error make { msg: $"Failed to create group ($name): ($r.status)" }
}

def ensure_user [user: record, group_ids: list<string>] {
  let existing = get_all_users | where username == $user.username | first --skip-empty
  let body = {
    username: $user.username
    email: $user.email
    firstName: $user.firstName
    lastName: $user.lastName
    isAdmin: $user.isAdmin
  }
  
  if $existing != null {
    let r = api "put" $"/users/($existing.id)" $body
    if $r.status != 200 { error make { msg: $"Failed to update user ($user.username): ($r.status)" } }
    let user_id = $existing.id
    api "put" $"/users/($user_id)/user-groups" { userGroupIds: $group_ids } | ignore
    return $user_id
  }
  
  let r = api "post" "/users" $body
  if $r.status != 201 { error make { msg: $"Failed to create user ($user.username): ($r.status)" } }
  let user_id = $r.body.id
  api "put" $"/users/($user_id)/user-groups" { userGroupIds: $group_ids } | ignore
  $user_id
}

wait_ready

# Create groups and build name->id map
let group_map = $groups | each { |g|
  let id = ensure_group $g.name
  { name: $g.name, id: $id }
} | reduce --fold {} { |it, acc| $acc | insert $it.name $it.id }

# Create users and assign groups
$users | each { |u|
  let group_ids = $u.groups | each { |g| $group_map | get $g }
  ensure_user $u $group_ids
} | ignore
