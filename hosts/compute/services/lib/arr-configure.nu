#!/usr/bin/env nu

# Generic *arr service initialization (Radarr/Sonarr).

let arr_name = $env.ARR_NAME
let base_url = $env.ARR_URL
let api_key = open $env.ARR_API_KEY_FILE | str trim
let config = open $env.ARR_CONFIG_FILE
let category_field = $env.ARR_CATEGORY_FIELD
let ntfy_token = open $env.NTFY_TOKEN_FILE | str trim
let headers = [X-Api-Key $api_key]

def wait_ready [] {
  for attempt in 1..30 {
    print $"Waiting for ($arr_name)... ($attempt)"
    let r = try { http get $"($base_url)/api/v3/system/status" --headers $headers --full --allow-errors } catch { null }
    if $r != null and $r.status == 200 { return }
    sleep 2sec
  }
  error make { msg: $"($arr_name) failed to start after 30 attempts" }
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

  let default_profile_name = $config.defaultQualityProfile
  let default_profile_id = get_quality_profile_id $default_profile_name

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

  let existing_clients = ($existing.body | default [])
  let dc = $config | get downloadClient

  # Disable other Transmission clients that don't match our configured name
  let stale_clients = ($existing_clients | where implementation == "Transmission" | where name != $dc.name | where enable == true)
  for client in $stale_clients {
    print $"  Disabling stale download client: ($client.name)"
    let r = http put $"($base_url)/api/v3/downloadclient/($client.id)" ($client | merge { enable: false }) --headers $headers --content-type application/json --full --allow-errors
    if $r.status != 202 {
      error make { msg: $"Failed to disable download client ($client.name): ($r.status) - ($r.body)" }
    }
  }

  let schemas = http get $"($base_url)/api/v3/downloadclient/schema" --headers $headers --full --allow-errors
  if $schemas.status != 200 {
    error make { msg: $"Failed to get download client schemas: ($schemas.status)" }
  }

  let transmission_schema = ($schemas.body | where implementation == "Transmission" | get 0?)
  if $transmission_schema == null {
    error make { msg: "Transmission schema not found" }
  }

  let fields = $transmission_schema.fields | each { |f|
    if $f.name == "host" {
      $f | upsert value $dc.host
    } else if $f.name == "port" {
      $f | upsert value $dc.port
    } else if $f.name == "urlBase" {
      $f | upsert value $dc.urlBase
    } else if $f.name == $category_field {
      $f | upsert value $dc.category
    } else {
      $f
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

  # Update if exists, create otherwise
  let existing_client = ($existing_clients | where name == $dc.name | get 0?)
  if $existing_client != null {
    let r = http put $"($base_url)/api/v3/downloadclient/($existing_client.id)" ($payload | merge { id: $existing_client.id }) --headers $headers --content-type application/json --full --allow-errors
    if $r.status != 202 {
      error make { msg: $"Failed to update download client ($dc.name): ($r.status) - ($r.body)" }
    }
    print $"  Updated download client: ($dc.name)"
  } else {
    let r = http post $"($base_url)/api/v3/downloadclient" $payload --headers $headers --content-type application/json --full --allow-errors
    if $r.status not-in [200, 201] {
      error make { msg: $"Failed to create download client ($dc.name): ($r.status) - ($r.body)" }
    }
    print $"  Created download client: ($dc.name)"
  }
}

def ensure_notification [] {
  print "Configuring ntfy notification..."
  let existing = http get $"($base_url)/api/v3/notification" --headers $headers --full --allow-errors
  if $existing.status != 200 {
    error make { msg: $"Failed to get notifications: ($existing.status) - ($existing.body)" }
  }

  let notification_name = "ntfy"
  let existing_names = ($existing.body | default [] | get -o name | default [])
  if $notification_name in $existing_names {
    print $"  Notification exists: ($notification_name)"
    return
  }

  let schemas = http get $"($base_url)/api/v3/notification/schema" --headers $headers --full --allow-errors
  if $schemas.status != 200 {
    error make { msg: $"Failed to get notification schemas: ($schemas.status)" }
  }

  let ntfy_schema = ($schemas.body | where implementation == "Ntfy" | get 0?)
  if $ntfy_schema == null {
    error make { msg: "Ntfy notification schema not found" }
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
    onDownload: true
    onUpgrade: true
    onGrab: false
    onRename: false
    onHealthIssue: false
    onApplicationUpdate: false
  }

  let r = http post $"($base_url)/api/v3/notification" $payload --headers $headers --content-type application/json --full --allow-errors
  if $r.status not-in [200, 201] {
    error make { msg: $"Failed to create notification ($notification_name): ($r.status) - ($r.body)" }
  }
  print $"  Created notification: ($notification_name)"
}

def ensure_default_delay_profile [] {
  print "Configuring default delay profile..."
  let profile = ($config | get -o defaultDelayProfile)
  if $profile == null {
    print "  No default delay profile config - skipping"
    return
  }

  let existing = http get $"($base_url)/api/v3/delayprofile" --headers $headers --full --allow-errors
  if $existing.status != 200 {
    error make { msg: $"Failed to get delay profiles: ($existing.status) - ($existing.body)" }
  }

  let defaults = ($existing.body | where tags == [])
  if ($defaults | length) != 1 {
    print $"  Warning: Expected exactly one default delay profile, found ($defaults | length) - skipping"
    return
  }

  let existing_profile = $defaults | get 0
  let payload = ($existing_profile | merge {
    enableUsenet: $profile.enableUsenet
    enableTorrent: $profile.enableTorrent
    preferredProtocol: $profile.preferredProtocol
    usenetDelay: $profile.usenetDelay
    torrentDelay: $profile.torrentDelay
    bypassIfHighestQuality: $profile.bypassIfHighestQuality
  })
  let r = http put $"($base_url)/api/v3/delayprofile/($existing_profile.id)" $payload --headers $headers --content-type application/json --full --allow-errors
  if $r.status not-in [200, 202] {
    error make { msg: $"Failed to update delay profile: ($r.status) - ($r.body)" }
  }
  print $"  Updated default delay profile: torrent delay ($profile.torrentDelay)min, usenet delay ($profile.usenetDelay)min"
}

def main [] {
  wait_ready
  print $"($arr_name) is ready"

  ensure_root_folders
  ensure_download_client
  ensure_notification
  ensure_default_delay_profile

  print $"($arr_name) initialization complete"
}
