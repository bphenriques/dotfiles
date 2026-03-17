#!/usr/bin/env nu
# Provisions a single Pocket-ID OIDC client.
# Generates a new secret only when the client is newly created or local credentials are missing.
#
# Environment variables:
#   POCKET_ID_URL - Pocket-ID base URL
#   POCKET_ID_API_KEY_FILE - Path to file containing API key
#   OIDC_CLIENT_CONFIG_FILE - Path to JSON config for this client
#   OIDC_CREDENTIALS_DIR - Base directory for OIDC credentials (e.g., /run/homelab-oidc)

const PAGINATION_LIMIT = 100

let base_url = $env.POCKET_ID_URL
let api_key = open $env.POCKET_ID_API_KEY_FILE | str trim
let client = open $env.OIDC_CLIENT_CONFIG_FILE
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

def write_credential [path: string, content: string, group: string] {
  let tmp = $"($path).tmp"
  $content | save --force $tmp
  chmod 0640 $tmp
  chown $"root:($group)" $tmp
  mv --force $tmp $path  # atomic rename
}

def main [] {
  wait_ready

  let client_dir = $"($credentials_dir)/($client.name)"
  let client_group = $"homelab-oidc-($client.name)"
  let id_file = $"($client_dir)/id"
  let secret_file = $"($client_dir)/secret"

  print $"Provisioning OIDC client: ($client.name)"

  let existing_clients = get_all "oidc/clients"
  let found = $existing_clients | where name == $client.name | get 0?

  let desired = {
    name: $client.name
    callbackURLs: ($client.callbackURLs | sort)
    pkceEnabled: $client.pkce
    isPublic: $client.pkce  # PKCE clients are public (browser-based SPAs)
  }

  let is_new = $found == null

  let client_id = if $found != null {
    let dominated_fields = $desired | columns
    let current = $found | select ...$dominated_fields | update callbackURLs { sort }

    if $current != $desired {
      let r = http put $"($base_url)/api/oidc/clients/($found.id)" $desired --headers $headers --content-type application/json --full --allow-errors
      if $r.status != 200 {
        error make { msg: $"Failed to update OIDC client ($client.name): ($r.status) - ($r.body)" }
      }
      print $"  Updated OIDC client config: ($client.name)"
    } else {
      print $"  OIDC client config up-to-date: ($client.name)"
    }
    $found.id
  } else {
    let r = http post $"($base_url)/api/oidc/clients" $desired --headers $headers --content-type application/json --full --allow-errors
    if $r.status != 201 {
      error make { msg: $"Failed to create OIDC client ($client.name): ($r.status) - ($r.body)" }
    }
    print $"  Created OIDC client: ($client.name)"
    $r.body.id
  }

  # Detect if local credentials are missing or stale (id mismatch means server-side recreate)
  let local_id = if ($id_file | path exists) { open $id_file | str trim } else { null }
  let credentials_missing = not ($id_file | path exists) or not ($secret_file | path exists)
  let id_mismatch = $local_id != null and $local_id != $client_id
  let needs_secret = $is_new or $credentials_missing or $id_mismatch

  if $id_mismatch { print $"  Local id mismatch, forcing secret rotation for: ($client.name)" }

  # Generate new secret only when needed
  let client_secret = if $needs_secret {
    let secret_r = http post $"($base_url)/api/oidc/clients/($client_id)/secret" "" --headers $headers --content-type application/json --full --allow-errors
    if $secret_r.status != 200 {
      error make { msg: $"Failed to generate secret for OIDC client ($client.name): ($secret_r.status) - ($secret_r.body)" }
    }
    print $"  Generated new secret for: ($client.name)"
    $secret_r.body.secret
  } else {
    print $"  Secret exists, skipping rotation for: ($client.name)"
    null
  }

  # Write credentials
  mkdir $client_dir
  chmod 0750 $client_dir
  chown $"root:($client_group)" $client_dir

  if $needs_secret {
    write_credential $id_file ($client_id | into string) $client_group
    write_credential $secret_file $client_secret $client_group
  }

  print $"  Credentials ready at ($client_dir)"
  print $"OIDC client ($client.name) provisioning complete"
}
