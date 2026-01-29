#!/usr/bin/env nu

# Initializes users and groups. New users are invited.
#
# Limitation: renaming users will create a new one.
const PAGINATION_LIMIT = 100

let base_url = $env.POCKET_ID_URL
let api_key = open $env.POCKET_ID_API_KEY_FILE | str trim
let config = open $env.POCKET_ID_CONFIG_FILE
let headers = { "X-API-KEY": $api_key, "Content-Type": "application/json" }

# OIDC credentials config (from Nix)
let CREDENTIALS_DIR = $env.HOMELAB_OIDC_CREDENTIALS_DIR
let CREDENTIALS_PLACEHOLDER = $env.HOMELAB_OIDC_PLACEHOLDER

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

# Verifies that credentials for a client are set (not placeholder or empty).
# If this fails, it means the client exists in Pocket-ID but local files are missing.
# Recovery: delete client from Pocket-ID, delete /var/lib/homelab-oidc/{name}/, restart pocket-id-init.
def assert_credentials_set [name: string] {
  let client_dir = $"($CREDENTIALS_DIR)/($name)"
  let id_val = (open $"($client_dir)/id" | str trim)
  let secret_val = (open $"($client_dir)/secret" | str trim)

  if $id_val == $CREDENTIALS_PLACEHOLDER or $id_val == "" {
    error make { msg: $"OIDC client ($name): id is still placeholder or empty" }
  }
  if $secret_val == $CREDENTIALS_PLACEHOLDER or $secret_val == "" {
    error make { msg: $"OIDC client ($name): secret is still placeholder or empty" }
  }
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
      let body = $desired | to json
      let r = http put $"($base_url)/api/oidc/clients/($found.id)" $body --headers $headers --full --allow-errors
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
  let body = $desired | to json
  let r = http post $"($base_url)/api/oidc/clients" $body --headers $headers --full --allow-errors
  if $r.status != 201 {
    error make { msg: $"Failed to create OIDC client ($client.name): ($r.status) - ($r.body)" }
  }

  # Create and generate a secret for the new client
  let client_id = $r.body.id
  let secret_r = http post $"($base_url)/api/oidc/clients/($client_id)/secret" "" --headers $headers --full --allow-errors
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

  $client_id | save --force $"($client_dir)/id"
  chmod 0640 $"($client_dir)/id"
  chown $"root:($client_group)" $"($client_dir)/id"

  $client_secret | save --force $"($client_dir)/secret"
  chmod 0640 $"($client_dir)/secret"
  chown $"root:($client_group)" $"($client_dir)/secret"

  print $"Wrote credentials to ($client_dir)"
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

  # Verify all credentials are provisioned (not placeholders).
  # This can happen if a client exists in Pocket-ID but local files were deleted/reset.
  # TODO: Consider adding a "rotate" flag in Nix to force secret regeneration when
  # local files are lost but client exists in Pocket-ID. For now, fail fast.
  $clients_cfg | each { |c| assert_credentials_set $c.name } | ignore

  print "Pocket ID initialization complete"
}
