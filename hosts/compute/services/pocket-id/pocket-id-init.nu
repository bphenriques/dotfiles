#!/usr/bin/env nu

# Initializes users and groups. New users are invited.
#
# Limitation: renaming users will create a new one.
const PAGINATION_LIMIT = 100

let base_url = $env.POCKET_ID_URL
let api_key = open $env.POCKET_ID_API_KEY_FILE | str trim
let config = open $env.POCKET_ID_CONFIG_FILE
let headers = { "X-API-KEY": $api_key, "Content-Type": "application/json" }

def wait_ready [] {
  print "Waiting for Pocket ID to be ready..."
  for _ in 1..30 {
    try {
      let r = http get $"($base_url)/.well-known/openid-configuration" --max-time 2sec --full --allow-errors
      if $r.status == 200 { print "Pocket ID is ready"; return }
    } catch { }
    sleep 1sec
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

  let body = { name: $group.name, friendlyName: $group.name, customClaims: [{ key: "managed-by", value: "nix" }] } | to json
  let r = http post $"($base_url)/api/user-groups" $body --headers $headers --full --allow-errors
  if $r.status != 201 { error make { msg: $"Failed to create group ($group.name): ($r.status) - ($r.body)" } }
  print $"Created group: ($group.name)"
  $r.body.id
}

def ensure_user [user: record, group_ids: list<string>, existing: list] {
  let found = $existing | where username == $user.username | get 0?
  let body = {
    username: $user.username
    email: $user.email
    firstName: $user.firstName
    lastName: $user.lastName
    displayName: $"($user.firstName) ($user.lastName)"
    isAdmin: $user.isAdmin
    customClaims: [{ key: "managed-by", value: "nix" }]
  } | to json

  let user_id = if $found != null {
    let r = http put $"($base_url)/api/users/($found.id)" $body --headers $headers --full --allow-errors
    if $r.status != 200 { error make { msg: $"Failed to update user ($user.username): ($r.status) - ($r.body)" } }
    $found.id
  } else {
    let r = http post $"($base_url)/api/users" $body --headers $headers --full --allow-errors
    if $r.status != 201 { error make { msg: $"Failed to create user ($user.username): ($r.status) - ($r.body)" } }
    print $"Created user: ($user.username)"
    let ir = http post $"($base_url)/api/users/($r.body.id)/one-time-access-email" "{}" --headers $headers --full --allow-errors
    if $ir.status == 204 {
      print $"Sent invite email to ($user.email)"
    } else {
      print $"Failed to send invite email: ($ir.status) - ($ir.body)"
    }
    $r.body.id
  }

  let group_body = { userGroupIds: $group_ids } | to json
  let gr = http put $"($base_url)/api/users/($user_id)/user-groups" $group_body --headers $headers --full --allow-errors
  if $gr.status != 200 { error make { msg: $"Failed to set groups for ($user.username): ($gr.status) - ($gr.body)" } }
  $user_id
}

def parse_env_file [path: string] {
  open $path
  | lines
  | where $it != '' and not ($it | str starts-with '#')
  | parse --regex '^(?<key>[^=]+)=(?<value>.*)$'
  | transpose --header-row --as-record
}

def ensure_oidc_client [client: record, existing: list] {
  let found = $existing | where name == $client.name | get 0?
  let creds = parse_env_file $client.credentialsFile

  # Compute the desired state, anything that isn't relevant is discarded.
  let desired = $client | reject credentialsFile | merge { isPublic: false }

  if $found != null {
    # Compare only the fields we care about (ignore id, createdAt, etc from API)
    let dominated_fields = $desired | columns
    let current = $found | select ...$dominated_fields

    if $current != $desired {
      let body = $desired | to json
      let r = http put $"($base_url)/api/oidc/clients/($found.id)" $body --headers $headers --full --allow-errors
      if $r.status != 200 {
        error make { msg: $"Failed to update OIDC client ($client.name): ($r.status) - ($r.body)" }
      }
      print $"Updated OIDC client: ($client.name)"
    } else {
      print $"OIDC client up-to-date: ($client.name)"
    }

    return $found.id
  }

  # Create new client with credentials from SOPS
  let body = $desired | merge {
    credentials: {
      clientId: ($creds | get OAUTH2_CLIENT_ID),
      clientSecret: ($creds | get OAUTH2_CLIENT_SECRET)
    }
  } | to json

  let r = http post $"($base_url)/api/oidc/clients" $body --headers $headers --full --allow-errors
  if $r.status != 201 {
    error make { msg: $"Failed to create OIDC client ($client.name): ($r.status) - ($r.body)" }
  }

  print $"Created OIDC client: ($client.name)"
  $r.body.id
}

def main [] {
  wait_ready

  print $"Config groups: ($config.groups | get name)"
  print $"Config users: ($config.users | get username)"

  let existing_groups = get_all "user-groups"
  print $"Existing groups: ($existing_groups)"

  let group_map = $config.groups 
    | each { |g| { name: $g.name, id: (ensure_group $g $existing_groups) } } 
    | transpose -rd

  let existing_users = get_all "users"
  print $"Existing users: ($existing_users)"

  $config.users | each { |u|
    let group_ids = $u.groups | each { |g| $group_map | get $g }
    ensure_user $u $group_ids $existing_users
  } | ignore

  print "Pocket ID users and groups initialization complete"

  # Handle OIDC clients if defined
  let clients_cfg = $config | get clients? | default []
  if ($clients_cfg | is-empty) {
    print "No OIDC clients defined; skipping client provisioning"
    return
  }

  print $"Config OIDC clients: ($clients_cfg | get name)"

  let existing_clients = get_all "oidc/clients"
  print $"Existing OIDC clients: ($existing_clients | get name)"

  $clients_cfg | each { |c| ensure_oidc_client $c $existing_clients } | ignore

  print "Pocket ID initialization complete"
}
