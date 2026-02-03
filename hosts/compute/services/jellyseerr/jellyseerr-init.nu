#!/usr/bin/env nu

# Initializes Jellyseerr declaratively via the API.
# - Pre-seeds settings.json to skip the setup wizard (if missing)
# - Configures Radarr and Sonarr server connections
#
# Limitations:
# - Only creates entities if missing;
# - Won't reconcile whenever there is a config drift.

let settings_file = "/var/lib/jellyseerr/settings.json"
let settings_template = $env.JELLYSEERR_SETTINGS_TEMPLATE
let base_url = $env.JELLYSEERR_URL
let api_key = open $env.JELLYSEERR_API_KEY_FILE | str trim
let jellyfin_admin_username = open $env.JELLYFIN_ADMIN_USERNAME_FILE | str trim
let jellyfin_admin_password = open $env.JELLYFIN_ADMIN_PASSWORD_FILE | str trim
let radarr_api_key = open $env.RADARR_API_KEY_FILE | str trim
let sonarr_api_key = open $env.SONARR_API_KEY_FILE | str trim
let config = open $env.JELLYSEERR_CONFIG_FILE
let headers = [X-Api-Key $api_key]

def authenticate_jellyfin []: nothing -> string {
  let jellyfin_url = $config.jellyfin.url
  print "Authenticating to Jellyfin..."
  let r = http post $"($jellyfin_url)/Users/AuthenticateByName" {
    Username: $jellyfin_admin_username
    Pw: $jellyfin_admin_password
  } --content-type application/json --headers [
    Authorization "MediaBrowser Client=\"jellyseerr-init\", Device=\"nix\", DeviceId=\"jellyseerr-init\", Version=\"1\""
  ] --full --allow-errors

  if $r.status != 200 {
    error make { msg: $"Failed to authenticate to Jellyfin: ($r.status) - ($r.body)" }
  }

  $r.body.AccessToken
}

def get_jellyfin_api_key [headers: list]: nothing -> record {
  let jellyfin_url = $config.jellyfin.url
  let r = http get $"($jellyfin_url)/Auth/Keys" --headers $headers --full --allow-errors
  if $r.status != 200 {
    error make { msg: $"Failed to get API keys: ($r.status) - ($r.body)" }
  }
  $r.body.Items | default [] | where AppName == "jellyseerr" | get 0?
}

def get_or_create_jellyfin_api_key [access_token: string]: nothing -> string {
  let jellyfin_url = $config.jellyfin.url
  print "Getting/creating Jellyfin API key for Jellyseerr..."
  let headers = [Authorization $"MediaBrowser Token=\"($access_token)\""]

  let existing_key = get_jellyfin_api_key $headers
  if $existing_key != null {
    print "  Using existing Jellyfin API key"
    return $existing_key.AccessToken
  }

  let r = http post $"($jellyfin_url)/Auth/Keys?app=jellyseerr" { } --headers $headers --content-type application/json --full --allow-errors
  if $r.status not-in [200, 204] {
    error make { msg: $"Failed to create API key: ($r.status) - ($r.body)" }
  }

  # POST returns 204 with no body, so we need to fetch the created key
  let created_key = get_jellyfin_api_key $headers
  if $created_key == null {
    error make { msg: "Failed to retrieve created API key" }
  }

  print "  Created new Jellyfin API key"
  $created_key.AccessToken
}

def preseed_settings [jellyfin_api_key: string] {
  if ($settings_file | path exists) {
    print "Settings file already exists, skipping pre-seed"
    return
  }

  print "Pre-seeding settings.json..."
  let template = open $settings_template
  let settings = $template
    | str replace "__API_KEY__" $api_key
    | str replace "__JELLYFIN_API_KEY__" $jellyfin_api_key

  $settings | save --force $settings_file
  print "  Settings pre-seeded successfully"
}

def wait_ready [] {
  print "Waiting for Jellyseerr..."
  for attempt in 1..30 {
    let r = try { http get $"($base_url)/api/v1/status" --headers $headers --full --allow-errors } catch { null }
    if $r != null and $r.status == 200 { return }
    sleep 2sec
  }
  error make { msg: "Jellyseerr failed to start after 30 attempts" }
}

def get_quality_profile_id [service_url: string, service_api_key: string, profile_name: string] {
  let profiles = http get $"($service_url)/api/v3/qualityprofile" --headers [X-Api-Key $service_api_key] --full --allow-errors
  if $profiles.status != 200 {
    error make { msg: $"Failed to get quality profiles: ($profiles.status) - ($profiles.body)" }
  }
  let profile = ($profiles.body | where name == $profile_name | get 0?)
  if $profile == null {
    error make { msg: $"Quality profile '($profile_name)' not found" }
  }
  $profile.id
}

def ensure_radarr [] {
  print "Configuring Radarr server..."
  let existing = http get $"($base_url)/api/v1/settings/radarr" --headers $headers --full --allow-errors
  if $existing.status != 200 {
    error make { msg: $"Failed to get Radarr servers: ($existing.status) - ($existing.body)" }
  }

  let radarr_cfg = $config | get radarr
  let existing_names = ($existing.body | default [] | get -o name | default [])

  if $radarr_cfg.name in $existing_names {
    print $"  Radarr server exists: ($radarr_cfg.name)"
    return
  }

  let radarr_url = $"http://($radarr_cfg.hostname):($radarr_cfg.port)"
  let profile_id = get_quality_profile_id $radarr_url $radarr_api_key $radarr_cfg.activeProfileName

  let payload = {
    name: $radarr_cfg.name
    hostname: $radarr_cfg.hostname
    port: $radarr_cfg.port
    apiKey: $radarr_api_key
    useSsl: $radarr_cfg.useSsl
    baseUrl: $radarr_cfg.baseUrl
    activeProfileId: $profile_id
    activeProfileName: $radarr_cfg.activeProfileName
    activeDirectory: $radarr_cfg.activeDirectory
    is4k: $radarr_cfg.is4k
    minimumAvailability: $radarr_cfg.minimumAvailability
    isDefault: $radarr_cfg.isDefault
    externalUrl: $radarr_cfg.externalUrl
  }

  let r = http post $"($base_url)/api/v1/settings/radarr" $payload --headers $headers --content-type application/json --full --allow-errors
  if $r.status not-in [200, 201] {
    error make { msg: $"Failed to create Radarr server ($radarr_cfg.name): ($r.status) - ($r.body)" }
  }
  print $"  Created Radarr server: ($radarr_cfg.name)"
}

def ensure_sonarr [] {
  print "Configuring Sonarr server..."
  let existing = http get $"($base_url)/api/v1/settings/sonarr" --headers $headers --full --allow-errors
  if $existing.status != 200 {
    error make { msg: $"Failed to get Sonarr servers: ($existing.status) - ($existing.body)" }
  }

  let sonarr_cfg = $config | get sonarr
  let existing_names = ($existing.body | default [] | get -o name | default [])

  if $sonarr_cfg.name in $existing_names {
    print $"  Sonarr server exists: ($sonarr_cfg.name)"
    return
  }

  let sonarr_url = $"http://($sonarr_cfg.hostname):($sonarr_cfg.port)"
  let profile_id = get_quality_profile_id $sonarr_url $sonarr_api_key $sonarr_cfg.activeProfileName

  let payload = {
    name: $sonarr_cfg.name
    hostname: $sonarr_cfg.hostname
    port: $sonarr_cfg.port
    apiKey: $sonarr_api_key
    useSsl: $sonarr_cfg.useSsl
    baseUrl: $sonarr_cfg.baseUrl
    activeProfileId: $profile_id
    activeProfileName: $sonarr_cfg.activeProfileName
    activeDirectory: $sonarr_cfg.activeDirectory
    is4k: $sonarr_cfg.is4k
    isDefault: $sonarr_cfg.isDefault
    externalUrl: $sonarr_cfg.externalUrl
  }

  let r = http post $"($base_url)/api/v1/settings/sonarr" $payload --headers $headers --content-type application/json --full --allow-errors
  if $r.status not-in [200, 201] {
    error make { msg: $"Failed to create Sonarr server ($sonarr_cfg.name): ($r.status) - ($r.body)" }
  }
  print $"  Created Sonarr server: ($sonarr_cfg.name)"
}

def main [] {
  let access_token = authenticate_jellyfin
  let jellyfin_api_key = get_or_create_jellyfin_api_key $access_token
  preseed_settings $jellyfin_api_key

  wait_ready
  print "Jellyseerr is ready"

  ensure_radarr
  ensure_sonarr

  print "Jellyseerr initialization complete"
}
