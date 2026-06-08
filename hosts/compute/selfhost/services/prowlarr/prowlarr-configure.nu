#!/usr/bin/env nu
# Initializes Prowlarr declaratively via the API:
# - Creates entities if missing;
# - Reconciles applications whenever there is a config drift;
# - Forces an indexer sync to propagate changes to connected apps.
let base_url = $env.PROWLARR_URL
let api_key = open $env.PROWLARR_API_KEY_FILE | str trim
let radarr_api_key = open $env.RADARR_API_KEY_FILE | str trim
let sonarr_api_key = open $env.SONARR_API_KEY_FILE | str trim
let ntfy_token = open $env.NTFY_TOKEN_FILE | str trim
let config = open $env.PROWLARR_CONFIG_FILE
let headers = [X-Api-Key, $api_key]
def wait_ready [] {
  for attempt in 1..30 {
    print $"Waiting for Prowlarr... ($attempt)"
    let r = try {
      http get $"($base_url)/api/v1/system/status" --headers $headers
      return
    } catch { null }
    sleep 2sec
  }
  error make {msg: "Prowlarr failed to start after 30 attempts"}
}
def ensure_tag [tag_name: string]: nothing -> int {
  let existing = http get $"($base_url)/api/v1/tag" --headers $headers --full --allow-errors
  if $existing.status != 200 {
    error make {
      msg: $"Failed to get tags: ($existing.status) - ($existing.body)"
    }
  }
  let tag = (
    $existing.body | default [] | where label == $tag_name | get 0?
  )
  if $tag != null {
    return $tag.id
  }
  let r = http post $"($base_url)/api/v1/tag" { label: $tag_name } --headers $headers --content-type application/json --full --allow-errors
  if $r.status not-in [200, 201] {
    error make {
      msg: $"Failed to create tag ($tag_name): ($r.status) - ($r.body)"
    }
  }
  print $"  Created tag: ($tag_name)"
  $r.body.id
}
def get_default_app_profile_id [] {
  let profiles = http get $"($base_url)/api/v1/appprofile" --headers $headers --full --allow-errors
  if $profiles.status != 200 {
    error make {
      msg: $"Failed to get app profiles: ($profiles.status) - ($profiles.body)"
    }
  }
  let profile = ($profiles.body | get 0?)
  if $profile == null {
    error make {msg: "No app profiles found in Prowlarr"}
  }
  $profile.id
}
def ensure_indexers [] {
  print "Configuring indexers..."
  let indexers = ($config | get -o indexers) | default []
  if ($indexers | is-empty) {
    error make {
      msg: $"No indexers defined."
    }
  }
  let existing = http get $"($base_url)/api/v1/indexer" --headers $headers --full --allow-errors
  if $existing.status != 200 {
    error make {
      msg: $"Failed to get indexers: ($existing.status) - ($existing.body)"
    }
  }
  let existing_names = (
    $existing.body | default [] | get -o name | default []
  )
  # Get available indexer schemas
  let schemas = http get $"($base_url)/api/v1/indexer/schema" --headers $headers --full --allow-errors
  if $schemas.status != 200 {
    error make {
      msg: $"Failed to get indexer schemas: ($schemas.status)"
    }
  }
  let app_profile_id = get_default_app_profile_id
  mut failed = []
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
    # Resolve tag names to IDs
    let tag_names = ($idx | get -o tags) | default []
    let tag_ids = $tag_names | each { |t| ensure_tag $t }
    let payload = $schema | merge {
      name: $idx.name
      enable: true
      appProfileId: $app_profile_id
      fields: $fields
      tags: $tag_ids
    }
    let r = http post $"($base_url)/api/v1/indexer?forceSave=true" $payload --headers $headers --content-type application/json --full --allow-errors
    if $r.status not-in [200, 201] {
      print $"  Warning: Failed to create indexer ($idx.name): ($r.status) - ($r.body)"
      $failed = ($failed | append $idx.name)
    } else {
      print $"  Created indexer: ($idx.name)"
    }
  }
  if ($failed | is-not-empty) {
    print $"  Warning: Failed to create indexers: ($failed | str join ', ')"
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
    error make {
      msg: $"Failed to get applications: ($existing.status) - ($existing.body)"
    }
  }
  let schemas = http get $"($base_url)/api/v1/applications/schema" --headers $headers --full --allow-errors
  if $schemas.status != 200 {
    error make {
      msg: $"Failed to get application schemas: ($schemas.status)"
    }
  }
  for app in $applications {
    let existing_app = (
      $existing.body | default [] | where name == $app.name | get 0?
    )
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
    # Build fields from existing record (preserving server-set values) or schema defaults
    let base_fields = if $existing_app != null { $existing_app.fields } else { $schema.fields }
    let fields = $base_fields | each { |f|
      match $f.name {
        "baseUrl" => ($f | upsert value $app.baseUrl)
        "prowlarrUrl" => ($f | upsert value $app.prowlarrUrl)
        "apiKey" => ($f | upsert value $app_api_key)
        "syncLevel" => ($f | upsert value $app.syncLevel)
        _ => $f
      }
    }
    if $existing_app != null {
      let payload = ($existing_app | merge {
        syncLevel: $app.syncLevel
        fields: $fields
      })
      let r = http put $"($base_url)/api/v1/applications/($existing_app.id)" $payload --headers $headers --content-type application/json --full --allow-errors
      if $r.status not-in [200, 202] {
        error make {
          msg: $"Failed to update application ($app.name): ($r.status) - ($r.body)"
        }
      }
      print $"  Updated application: ($app.name)"
    } else {
      let payload = ($schema | merge {
        name: $app.name
        syncLevel: $app.syncLevel
        fields: $fields
        tags: []
      })
      let r = http post $"($base_url)/api/v1/applications" $payload --headers $headers --content-type application/json --full --allow-errors
      if $r.status not-in [200, 201] {
        error make {
          msg: $"Failed to create application ($app.name): ($r.status) - ($r.body)"
        }
      }
      print $"  Created application: ($app.name)"
    }
  }
}
def ensure_notification [] {
  print "Configuring ntfy notification..."
  let existing = http get $"($base_url)/api/v1/notification" --headers $headers --full --allow-errors
  if $existing.status != 200 {
    error make {
      msg: $"Failed to get notifications: ($existing.status) - ($existing.body)"
    }
  }
  let notification_name = "ntfy"
  let existing_names = (
    $existing.body | default [] | get -o name | default []
  )
  if $notification_name in $existing_names {
    print $"  Notification exists: ($notification_name)"
    return
  }
  let schemas = http get $"($base_url)/api/v1/notification/schema" --headers $headers --full --allow-errors
  if $schemas.status != 200 {
    error make {
      msg: $"Failed to get notification schemas: ($schemas.status)"
    }
  }
  let ntfy_schema = ($schemas.body | where implementation == "Ntfy" | get 0?)
  if $ntfy_schema == null {
    error make {msg: "Ntfy notification schema not found"}
  }
  let ntfy_config = $config | get notification
  let fields = $ntfy_schema.fields | each { |f|
    match $f.name {
      "serverUrl" => ($f | upsert value $ntfy_config.serverUrl)
      "accessToken" => ($f | upsert value $ntfy_token)
      "topics" => ($f | upsert value $ntfy_config.topic)
      "tags" => ($f | upsert value ($ntfy_config | get -o tags | default ""))
      _ => $f
    }
  }
  let payload = {
    name: $notification_name
    enable: true
    fields: $fields
    implementationName: "Ntfy"
    implementation: "Ntfy"
    configContract: "NtfySettings"
    onHealthIssue: true
    onHealthRestored: true
    onGrab: false
    onApplicationUpdate: false
  }
  let r = http post $"($base_url)/api/v1/notification" $payload --headers $headers --content-type application/json --full --allow-errors
  if $r.status not-in [200, 201] {
    error make {
      msg: $"Failed to create notification ($notification_name): ($r.status) - ($r.body)"
    }
  }
  print $"  Created notification: ($notification_name)"
}
def sync_indexers [] {
  print "Syncing indexers to applications..."
  let r = http post $"($base_url)/api/v1/command" { name: "ApplicationIndexerSync" } --headers $headers --content-type application/json --full --allow-errors
  if $r.status not-in [200, 201, 202] {
    error make {
      msg: $"Failed to sync indexers: ($r.status) - ($r.body)"
    }
  }
  print "  Indexer sync triggered"
}
def main [] {
  wait_ready
  print "Prowlarr is ready"
  ensure_applications
  ensure_notification
  ensure_indexers
  sync_indexers
  print "Prowlarr initialization complete"
}
