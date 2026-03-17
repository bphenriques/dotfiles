#!/usr/bin/env nu
# Provisions Pocket-ID users and groups.
#
# Environment variables:
#   POCKET_ID_URL - Pocket-ID base URL
#   POCKET_ID_API_KEY_FILE - Path to file containing API key
#   OIDC_CONFIG_FILE - Path to JSON config with users and groups
#   OIDC_CREDENTIALS_DIR - Base directory for OIDC credentials (e.g., /run/homelab-oidc)

const PAGINATION_LIMIT = 100

let base_url = $env.POCKET_ID_URL
let api_key = open $env.POCKET_ID_API_KEY_FILE | str trim
let config = open $env.OIDC_CONFIG_FILE
let credentials_dir = $env.OIDC_CREDENTIALS_DIR
let headers = { "X-API-KEY": $api_key }

def wait_ready [] {
  for attempt in 1..60 {
    print $"Waiting for Pocket ID... ($attempt)"
    try { http get $"($base_url)/.well-known/openid-configuration" --max-time 2sec | ignore; return } catch { sleep 2sec }
  }
  error make { msg: "Pocket ID failed to start in time" }
}

def get_all [resource: string] {
  let r = http get $"($base_url)/api/($resource)?pagination[limit]=($PAGINATION_LIMIT)" --headers $headers --full --allow-errors
  if $r.status != 200 { error make { msg: $"Failed to list ($resource): ($r.status) - ($r.body)" } }
  $r.body.data
}

def ensure_group [group: record, existing: list] {
  let found = $existing | where name == $group.name | get 0?
  if $found != null { return $found.id }

  let body = { name: $group.name, friendlyName: $group.name, customClaims: [{ key: "managed-by", value: "nix" }] }
  let r = http post $"($base_url)/api/user-groups" $body --headers $headers --content-type application/json --full --allow-errors
  if $r.status != 201 { error make { msg: $"Failed to create group ($group.name): ($r.status) - ($r.body)" } }
  print $"  Created group: ($group.name)"
  $r.body.id
}

def ensure_user [user: record, group_ids: list<string>, existing: list] {
  let found = $existing | where username == $user.username | get 0?
  let body = {
    username: $user.username
    email: $user.email
    emailVerified: true
    firstName: $user.firstName
    lastName: $user.lastName
    displayName: $"($user.firstName) ($user.lastName)"
    isAdmin: $user.isAdmin
    customClaims: [{ key: "managed-by", value: "nix" }]
  }

  let user_id = if $found != null {
    let r = http put $"($base_url)/api/users/($found.id)" $body --headers $headers --content-type application/json --full --allow-errors
    if $r.status != 200 { error make { msg: $"Failed to update user ($user.username): ($r.status) - ($r.body)" } }
    $found.id
  } else {
    let r = http post $"($base_url)/api/users" $body --headers $headers --content-type application/json --full --allow-errors
    if $r.status != 201 { error make { msg: $"Failed to create user ($user.username): ($r.status) - ($r.body)" } }
    print $"  Created user: ($user.username)"

    # Send invite email
    let ir = http post $"($base_url)/api/users/($r.body.id)/one-time-access-email" "{}" --headers $headers --content-type application/json --full --allow-errors
    if $ir.status != 204 {
      error make { msg: $"Failed to send invite email to ($user.email): ($ir.status) - ($ir.body)" }
    }
    print $"  Sent invite email to ($user.email)"
    $r.body.id
  }

  # Update group membership
  let group_body = { userGroupIds: $group_ids }
  let gr = http put $"($base_url)/api/users/($user_id)/user-groups" $group_body --headers $headers --content-type application/json --full --allow-errors
  if $gr.status != 200 { error make { msg: $"Failed to set groups for ($user.username): ($gr.status) - ($gr.body)" } }

  { username: $user.username, id: $user_id }
}

def main [] {
  wait_ready
  print "Pocket ID is ready"

  # Provision groups
  print "Provisioning groups..."
  let existing_groups = get_all "user-groups"
  let group_map = $config.groups
    | each { |g| [$g.name (ensure_group $g $existing_groups)] }
    | into record

  # Provision users
  print "Provisioning users..."
  let existing_users = get_all "users"
  let provisioned_users = $config.users | each { |u|
    print $"Processing user: ($u.username)"
    let group_ids = $u.groups | each { |g| $group_map | get $g }
    ensure_user $u $group_ids $existing_users
  }

  # Write users mapping file
  let users_file = $"($credentials_dir)/oidc-users.json"
  let users_tmp = $"($users_file).tmp"
  $provisioned_users | to json | save --force $users_tmp
  chmod 0644 $users_tmp
  mv --force $users_tmp $users_file  # atomic rename
  print $"Wrote users mapping to ($users_file)"

  print "Pocket ID base provisioning complete"
}
