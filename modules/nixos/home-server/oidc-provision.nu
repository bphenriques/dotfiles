#!/usr/bin/env nu

# Provisions OIDC clients with Pocket-ID and writes credentials to tmpfs.
#
# This script is called on boot by homelab-oidc-provision.service.
# It ensures all configured OIDC clients exist and regenerates their secrets.
#
# Environment variables:
#   POCKET_ID_URL - Pocket-ID base URL
#   POCKET_ID_API_KEY_FILE - Path to file containing API key
#   OIDC_CONFIG_FILE - Path to JSON config with clients array
#   OIDC_CREDENTIALS_DIR - Base directory for credentials (e.g., /run/homelab-oidc)

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

def write_credential [path: string, content: string, group: string] {
  $content | save --force $path
  chmod 0640 $path
  chown $"root:($group)" $path
}

def provision_client [client: record, existing: list] {
  let found = $existing | where name == $client.name | get 0?

  # Compute the desired state
  let desired = {
    name: $client.name
    callbackURLs: $client.callbackURLs
    pkce: $client.pkce
    isPublic: false
  }

  let client_id = if $found != null {
    # Update existing client config
    let dominated_fields = $desired | columns
    let current = $found | select ...$dominated_fields

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
    # Create new client
    let r = http post $"($base_url)/api/oidc/clients" $desired --headers $headers --content-type application/json --full --allow-errors
    if $r.status != 201 {
      error make { msg: $"Failed to create OIDC client ($client.name): ($r.status) - ($r.body)" }
    }
    print $"  Created OIDC client: ($client.name)"
    $r.body.id
  }

  # Always regenerate secret (rotation on every provision)
  let secret_r = http post $"($base_url)/api/oidc/clients/($client_id)/secret" "" --headers $headers --content-type application/json --full --allow-errors
  if $secret_r.status != 200 {
    error make { msg: $"Failed to generate secret for OIDC client ($client.name): ($secret_r.status) - ($secret_r.body)" }
  }
  let client_secret = $secret_r.body.secret
  print $"  Generated new secret for: ($client.name)"

  # Write credentials with proper permissions
  let client_dir = $"($credentials_dir)/($client.name)"
  let client_group = $"homelab-oidc-($client.name)"

  mkdir $client_dir
  chmod 0750 $client_dir
  chown $"root:($client_group)" $client_dir

  write_credential $"($client_dir)/id" $client_id $client_group
  write_credential $"($client_dir)/secret" $client_secret $client_group

  print $"  Wrote credentials to ($client_dir)"
}

def main [] {
  wait_ready
  print "Pocket ID is ready"

  if ($config.clients | is-empty) {
    print "No OIDC clients configured"
    return
  }

  let existing = get_all "oidc/clients"
  for client in $config.clients {
    print $"Processing client: ($client.name)"
    provision_client $client $existing
  }

  print "OIDC provisioning complete"
}
