#!/usr/bin/env nu

# Initializes Radarr declaratively via the API.
#
# Limitations:
# - Only creates entities if missing;
# - Won't reconcile whenever there is a config drift.

let base_url = $env.RADARR_URL
let api_key = open $env.RADARR_API_KEY_FILE | str trim
let config = open $env.RADARR_CONFIG_FILE
let headers = [X-Api-Key $api_key]

def wait_ready [] {
  print "Waiting for Radarr..."
  for attempt in 1..30 {
    let r = try { http get $"($base_url)/api/v3/system/status" --headers $headers --full --allow-errors } catch { null }
    if $r != null and $r.status == 200 { return }
    sleep 2sec
  }
  error make { msg: "Radarr failed to start after 30 attempts" }
}

def get_quality_profile_id [name: string] {
  let profiles = http get $"($base_url)/api/v3/qualityprofile" --headers $headers --full --allow-errors
  if $profiles.status != 200 {
    error make { msg: $"Failed to get quality profiles: ($profiles.status) - ($profiles.body)" }
  }
  let profile = ($profiles.body | where name == $name | get 0?)
  if $profile == null {
    print $"  Warning: Quality profile '($name)' not found - skipping default profile assignment"
    return null
  }
  $profile.id
}

def ensure_root_folders [] {
  print "Configuring root folders..."
  let existing = http get $"($base_url)/api/v3/rootfolder" --headers $headers --full --allow-errors
  if $existing.status != 200 {
    error make { msg: $"Failed to get root folders: ($existing.status) - ($existing.body)" }
  }

  let default_profile_name = ($config | get -o defaultQualityProfile)
  let default_profile_id = if $default_profile_name != null {
    get_quality_profile_id $default_profile_name
  } else {
    null
  }

  let root_folders = ($config | get -o rootFolders) | default []
  for folder in $root_folders {
    let existing_folder = ($existing.body | default [] | where path == $folder.path | get 0?)

    if $existing_folder != null {
      print $"  Root folder exists: ($folder.path)"
    } else {
      let payload = if $default_profile_id != null {
        { path: $folder.path, defaultQualityProfileId: $default_profile_id }
      } else {
        { path: $folder.path }
      }
      let r = http post $"($base_url)/api/v3/rootfolder" $payload --headers $headers --content-type application/json --full --allow-errors
      if $r.status not-in [200, 201] {
        error make { msg: $"Failed to create root folder ($folder.path): ($r.status) - ($r.body)" }
      }
      print $"  Created root folder: ($folder.path)"
    }
  }
}

def ensure_download_client [] {
  print "Configuring download client..."
  let existing = http get $"($base_url)/api/v3/downloadclient" --headers $headers --full --allow-errors
  if $existing.status != 200 {
    error make { msg: $"Failed to get download clients: ($existing.status) - ($existing.body)" }
  }

  let existing_names = ($existing.body | default [] | get -o name | default [])

  let dc = $config | get downloadClient
  if $dc.name in $existing_names {
    print $"  Download client exists: ($dc.name)"
    return
  }

  # Get schema for Transmission
  let schemas = http get $"($base_url)/api/v3/downloadclient/schema" --headers $headers --full --allow-errors
  if $schemas.status != 200 {
    error make { msg: $"Failed to get download client schemas: ($schemas.status)" }
  }

  let transmission_schema = ($schemas.body | where implementation == "Transmission" | get 0?)
  if $transmission_schema == null {
    error make { msg: "Transmission schema not found" }
  }

  # Build the download client config
  let fields = $transmission_schema.fields | each { |f|
    match $f.name {
      "host" => ($f | upsert value $dc.host)
      "port" => ($f | upsert value $dc.port)
      "urlBase" => ($f | upsert value $dc.urlBase)
      "movieCategory" => ($f | upsert value $dc.category)
      _ => $f
    }
  }

  let payload = {
    enable: true
    protocol: "torrent"
    priority: 1
    removeCompletedDownloads: true
    removeFailedDownloads: true
    name: $dc.name
    fields: $fields
    implementationName: "Transmission"
    implementation: "Transmission"
    configContract: "TransmissionSettings"
  }

  let r = http post $"($base_url)/api/v3/downloadclient" $payload --headers $headers --content-type application/json --full --allow-errors
  if $r.status not-in [200, 201] {
    error make { msg: $"Failed to create download client ($dc.name): ($r.status) - ($r.body)" }
  }
  print $"  Created download client: ($dc.name)"
}

def main [] {
  wait_ready
  print "Radarr is ready"

  ensure_root_folders
  ensure_download_client

  print "Radarr initialization complete"
}
