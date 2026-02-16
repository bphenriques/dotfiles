#!/usr/bin/env nu

# Initializes users and groups. New users are invited.
#
# Limitation: renaming users will create a new one.
const PAGINATION_LIMIT = 100

let base_url = $env.POCKET_ID_URL
let api_key = open $env.POCKET_ID_API_KEY_FILE | str trim
let config = open $env.POCKET_ID_CONFIG_FILE
let headers = { "X-API-KEY": $api_key }

# OIDC credentials config (from Nix)
let CREDENTIALS_DIR = $env.HOMELAB_OIDC_CREDENTIALS_DIR
let CREDENTIALS_PLACEHOLDER = $env.HOMELAB_OIDC_PLACEHOLDER

def wait_ready [] {
  for attempt in 1..30 {
    print $"Waiting for Pocket ID... ($attempt)"
    try { http get $"($base_url)/.well-known/openid-configuration" --max-time 2sec | ignore; return } catch { sleep 1sec }
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
  print $"Created group: ($group.name)"
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
    print $"Created user: ($user.username)"
    let ir = http post $"($base_url)/api/users/($r.body.id)/one-time-access-email" "{}" --headers $headers --content-type application/json --full --allow-errors
    if $ir.status != 204 {
      error make { msg: $"Failed to send invite email to ($user.email): ($ir.status) - ($ir.body)" }
    }
    print $"Sent invite email to ($user.email)"
    $r.body.id
  }

  let group_body = { userGroupIds: $group_ids }
  let gr = http put $"($base_url)/api/users/($user_id)/user-groups" $group_body --headers $headers --content-type application/json --full --allow-errors
  if $gr.status != 200 { error make { msg: $"Failed to set groups for ($user.username): ($gr.status) - ($gr.body)" } }
  { username: $user.username, id: $user_id }
}

def write_credential [path: string, content: string, group: string] {
  $content | save --force $path
  chmod 0640 $path
  chown $"root:($group)" $path
}

# Verifies that credentials for a client are set (not placeholder or empty).
# If this fails, it means the client exists in Pocket-ID but local files are missing.
# Recovery: delete client from Pocket-ID, delete /var/lib/homelab-oidc/{name}/, restart pocket-id-configure.
def assert_credentials_set [name: string] {
  let client_dir = $"($CREDENTIALS_DIR)/($name)"
  ["id", "secret"] | each { |field|
    let val = open $"($client_dir)/($field)" | str trim
    if $val in [$CREDENTIALS_PLACEHOLDER, ""] {
      error make { msg: $"OIDC client ($name): ($field) is still placeholder or empty" }
    }
  } | ignore
  print $"Verified credentials for ($name)"
}

def ensure_oidc_client [client: record, existing: list] {
  let found = $existing | where name == $client.name | get 0?

  # Compute the desired state, anything that isn't relevant is discarded.
  let desired = $client | merge { isPublic: false }

  if $found != null {
    # Compare only the fields we care about (ignore id, createdAt, credentials, etc from API)
    let dominated_fields = $desired | columns
    let current = $found | select ...$dominated_fields

    if $current != $desired {
      let r = http put $"($base_url)/api/oidc/clients/($found.id)" $desired --headers $headers --content-type application/json --full --allow-errors
      if $r.status != 200 {
        error make { msg: $"Failed to update OIDC client ($client.name): ($r.status) - ($r.body)" }
      }
      print $"Updated OIDC client: ($client.name)"
    } else {
      print $"OIDC client up-to-date: ($client.name)"
    }

    return
  }

  # Create new client - Pocket-ID will generate the client ID
  let r = http post $"($base_url)/api/oidc/clients" $desired --headers $headers --content-type application/json --full --allow-errors
  if $r.status != 201 {
    error make { msg: $"Failed to create OIDC client ($client.name): ($r.status) - ($r.body)" }
  }

  # Create and generate a secret for the new client
  let client_id = $r.body.id
  let secret_r = http post $"($base_url)/api/oidc/clients/($client_id)/secret" "" --headers $headers --content-type application/json --full --allow-errors
  if $secret_r.status != 200 {
    error make { msg: $"Failed to generate secret for OIDC client ($client.name): ($secret_r.status) - ($secret_r.body)" }
  }
  print $"Created OIDC client: ($client.name) with ID: ($client_id)"
  let client_secret = $secret_r.body.secret

  # Write credentials to files with proper permissions
  # Directory: owned by root:homelab-oidc-{name}, mode 0750
  # Files: owned by root:homelab-oidc-{name}, mode 0640
  let client_dir = $"($CREDENTIALS_DIR)/($client.name)"
  let client_group = $"homelab-oidc-($client.name)"
  mkdir $client_dir
  chmod 0750 $client_dir
  chown $"root:($client_group)" $client_dir

  write_credential $"($client_dir)/id" $client_id $client_group
  write_credential $"($client_dir)/secret" $client_secret $client_group

  print $"Wrote credentials to ($client_dir)"
}

def main [] {
  wait_ready
  print "Pocket ID is ready"

  let existing_groups = get_all "user-groups"
  let group_map = $config.groups
    | each { |g| [$g.name (ensure_group $g $existing_groups)] }
    | into record

  let existing_users = get_all "users"
  let provisioned_users = $config.users | each { |u|
    let group_ids = $u.groups | each { |g| $group_map | get $g }
    ensure_user $u $group_ids $existing_users
  }

  # Write oidc-users.json for other services to read (e.g., miniflux-configure)
  let users_file = $"($CREDENTIALS_DIR)/oidc-users.json"
  $provisioned_users | to json | save --force $users_file
  chmod 0644 $users_file
  print $"Wrote users mapping to ($users_file)"

  # Handle OIDC clients if defined
  let clients_cfg = $config | get clients? | default []
  if ($clients_cfg | is-empty) {
    print "No OIDC clients defined; skipping client provisioning"
    return
  }

  let existing_clients = get_all "oidc/clients"
  $clients_cfg | each { |c| ensure_oidc_client $c $existing_clients } | ignore

  # Verify all credentials are provisioned (not placeholders).
  # This can happen if a client exists in Pocket-ID but local files were deleted/reset.
  # TODO: Consider adding a "rotate" flag in Nix to force secret regeneration when
  # local files are lost but client exists in Pocket-ID. For now, fail fast.
  $clients_cfg | each { |c| assert_credentials_set $c.name } | ignore

  print "Pocket ID initialization complete"
}
