#!/usr/bin/env nu

# Initializes Prowlarr declaratively via the API.
#
# Limitations:
# - Only creates entities if missing;
# - Won't reconcile whenever there is a config drift.

let base_url = $env.PROWLARR_URL
let api_key = open $env.PROWLARR_API_KEY_FILE | str trim
let radarr_api_key = open $env.RADARR_API_KEY_FILE | str trim
let sonarr_api_key = open $env.SONARR_API_KEY_FILE | str trim
let config = open $env.PROWLARR_CONFIG_FILE
let headers = [X-Api-Key $api_key]

def wait_ready [] {
  print "Waiting for Prowlarr..."
  for attempt in 1..30 {
    let r = try { http get $"($base_url)/api/v1/system/status" --headers $headers; return } catch { null }
    sleep 2sec
  }
  error make { msg: "Prowlarr failed to start after 30 attempts" }
}

def get_default_app_profile_id [] {
  let profiles = http get $"($base_url)/api/v1/appprofile" --headers $headers --full --allow-errors
  if $profiles.status != 200 {
    error make { msg: $"Failed to get app profiles: ($profiles.status) - ($profiles.body)" }
  }
  let profile = ($profiles.body | get 0?)
  if $profile == null {
    error make { msg: "No app profiles found in Prowlarr" }
  }
  $profile.id
}

def ensure_indexers [] {
  print "Configuring indexers..."
  let indexers = ($config | get -o indexers) | default []
  if ($indexers | is-empty) {
    error make { msg: $"No indexers defined." }
  }

  let existing = http get $"($base_url)/api/v1/indexer" --headers $headers --full --allow-errors
  if $existing.status != 200 {
    error make { msg: $"Failed to get indexers: ($existing.status) - ($existing.body)" }
  }

  let existing_names = ($existing.body | default [] | get -o name | default [])

  # Get available indexer schemas
  let schemas = http get $"($base_url)/api/v1/indexer/schema" --headers $headers --full --allow-errors
  if $schemas.status != 200 {
    error make { msg: $"Failed to get indexer schemas: ($schemas.status)" }
  }

  let app_profile_id = get_default_app_profile_id

  for idx in $indexers {
    if $idx.name in $existing_names {
      print $"  Indexer exists: ($idx.name)"
      continue
    }

    # Find the schema for this indexer
    let schema = ($schemas.body | where definitionName == $idx.definitionName | get 0?)
    if $schema == null {
      print $"  Indexer schema not found: ($idx.definitionName), skipping"
      continue
    }

    # Merge custom fields
    let fields_cfg = ($idx | get -o fields) | default {}
    let fields = $schema.fields | each { |f|
      let custom_value = $fields_cfg | get -o $f.name
      if $custom_value != null { $f | upsert value $custom_value } else { $f }
    }

    let payload = $schema | merge {
      name: $idx.name
      enable: true
      appProfileId: $app_profile_id
      fields: $fields
    }

    let r = http post $"($base_url)/api/v1/indexer" $payload --headers $headers --content-type application/json --full --allow-errors
    if $r.status not-in [200, 201] {
      error make { msg: $"Failed to create indexer ($idx.name): ($r.status) - ($r.body)" }
    }
    print $"  Created indexer: ($idx.name)"
  }
}

def ensure_applications [] {
  print "Configuring applications..."
  let applications = ($config | get -o applications) | default []
  if ($applications | is-empty) {
    print "  No applications configured"
    return
  }

  let existing = http get $"($base_url)/api/v1/applications" --headers $headers --full --allow-errors
  if $existing.status != 200 {
    error make { msg: $"Failed to get applications: ($existing.status) - ($existing.body)" }
  }

  let existing_names = ($existing.body | default [] | get -o name | default [])

  let schemas = http get $"($base_url)/api/v1/applications/schema" --headers $headers --full --allow-errors
  if $schemas.status != 200 {
    error make { msg: $"Failed to get application schemas: ($schemas.status)" }
  }

  for app in $applications {
    if $app.name in $existing_names {
      print $"  Application exists: ($app.name)"
      continue
    }

    # Get the appropriate API key
    let app_api_key = match $app.implementation {
      "Radarr" => $radarr_api_key
      "Sonarr" => $sonarr_api_key
      _ => {
        print $"  Unsupported application implementation: ($app.implementation), skipping"
        continue
      }
    }

    # Find schema for this implementation
    let schema = ($schemas.body | where implementation == $app.implementation | get 0?)
    if $schema == null {
      print $"  Application schema not found: ($app.implementation), skipping"
      continue
    }

    # Build fields
    let fields = $schema.fields | each { |f|
      match $f.name {
        "baseUrl" => ($f | upsert value $app.baseUrl)
        "prowlarrUrl" => ($f | upsert value $app.prowlarrUrl)
        "apiKey" => ($f | upsert value $app_api_key)
        "syncLevel" => ($f | upsert value $app.syncLevel)
        _ => $f
      }
    }

    let payload = {
      name: $app.name
      syncLevel: $app.syncLevel
      fields: $fields
      implementationName: $app.implementation
      implementation: $app.implementation
      configContract: $"($app.implementation)Settings"
      tags: []
    }

    let r = http post $"($base_url)/api/v1/applications" $payload --headers $headers --content-type application/json --full --allow-errors
    if $r.status not-in [200, 201] {
      error make { msg: $"Failed to create application ($app.name): ($r.status) - ($r.body)" }
    }
    print $"  Created application: ($app.name)"
  }
}

def main [] {
  wait_ready
  print "Prowlarr is ready"

  ensure_indexers
  ensure_applications

  print "Prowlarr initialization complete"
}
