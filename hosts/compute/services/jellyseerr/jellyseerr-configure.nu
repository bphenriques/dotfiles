#!/usr/bin/env nu

# Initializes Jellyseerr (partially) declaratively via the API.
# - TODO: reconcile on config drift.

let base_url = $env.JELLYSEERR_URL
let api_key = open $env.JELLYSEERR_API_KEY_FILE | str trim
let jellyfin_admin_username = open $env.JELLYFIN_ADMIN_USERNAME_FILE | str trim
let jellyfin_admin_password = open $env.JELLYFIN_ADMIN_PASSWORD_FILE | str trim
let radarr_api_key = open $env.RADARR_API_KEY_FILE | str trim
let sonarr_api_key = open $env.SONARR_API_KEY_FILE | str trim
let config = open $env.JELLYSEERR_CONFIG_FILE
let headers = [X-Api-Key $api_key]

def wait_ready [] {
  for attempt in 1..30 {
    print $"Waiting for Jellyseerr... ($attempt)"
    try { http get $"($base_url)/api/v1/status" --max-time 2sec | ignore; return } catch { sleep 1sec }
  }
  error make { msg: "Jellyseerr failed to start after 30 attempts" }
}

def complete_setup_wizard [] {
  let r = http get $"($base_url)/api/v1/settings/public" --full --allow-errors
  let public_settings = if $r.status == 200 { $r.body } else { { initialized: false, mediaServerType: 0 } }

  if ($public_settings.initialized? | default false) {
    print "Jellyseerr is already initialized"
    return
  }

  let jellyfinServerType = 2 # mediaServerType: 0 = NOT_CONFIGURED, 2 = Jellyfin
  if ($public_settings.mediaServerType? | default 0) == $jellyfinServerType {
    print "Jellyfin already configured, completing initialization..."
    # Just need to login (without hostname) and mark as initialized
    let payload = {
      username: $jellyfin_admin_username
      password: $jellyfin_admin_password
    }
    let r = http post $"($base_url)/api/v1/auth/jellyfin" $payload --content-type application/json --full --allow-errors
    if $r.status not-in [200, 201] {
      error make { msg: $"Failed to login to Jellyfin: ($r.status) - ($r.body)" }
    }
    print "  Logged in successfully"
  } else {
    print "Completing setup wizard..."
    let jellyfin_cfg = $config.jellyfin

    let payload = {
      username: $jellyfin_admin_username
      password: $jellyfin_admin_password
      hostname: $jellyfin_cfg.hostname
      port: $jellyfin_cfg.port
      urlBase: $jellyfin_cfg.urlBase
      useSsl: $jellyfin_cfg.useSsl
      email: $"($jellyfin_admin_username)@localhost"
      serverType: $jellyfinServerType
    }

    let r = http post $"($base_url)/api/v1/auth/jellyfin" $payload --content-type application/json --full --allow-errors
    if $r.status not-in [200, 201] {
      error make { msg: $"Failed to complete Jellyfin setup: ($r.status) - ($r.body)" }
    }
    print "  Jellyfin configured and admin user created"
  }

  # Mark as initialized
  let init_r = http post $"($base_url)/api/v1/settings/initialize" {} --headers $headers --content-type application/json --full --allow-errors
  if $init_r.status not-in [200, 204] {
    error make { msg: $"  Warning: Failed to mark as initialized: ($init_r.status) - ($init_r.body)" }
  }
  print "  Setup wizard completed"
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

def ensure_arr_server [kind: string, cfg: record, api_key: string] {
  let endpoint = $kind | str downcase
  print $"Configuring ($kind) server..."

  let existing = http get $"($base_url)/api/v1/settings/($endpoint)" --headers $headers --full --allow-errors
  if $existing.status != 200 {
    error make { msg: $"Failed to get ($kind) servers: ($existing.status) - ($existing.body)" }
  }

  let existing_names = ($existing.body | default [] | get -o name | default [])
  if $cfg.name in $existing_names {
    print $"  ($kind) server exists: ($cfg.name)"
    return
  }

  let profile_id = get_quality_profile_id $"http://($cfg.hostname):($cfg.port)" $api_key $cfg.activeProfileName
  mut payload = {
    name: $cfg.name
    hostname: $cfg.hostname
    port: $cfg.port
    apiKey: $api_key
    useSsl: $cfg.useSsl
    baseUrl: $cfg.baseUrl
    activeProfileId: $profile_id
    activeProfileName: $cfg.activeProfileName
    activeDirectory: $cfg.activeDirectory
    is4k: $cfg.is4k
    isDefault: $cfg.isDefault
    externalUrl: $cfg.externalUrl
  }

  # Service-specific fields
  if $kind == "Radarr" {
    $payload = $payload | merge { minimumAvailability: $cfg.minimumAvailability }
  } else if $kind == "Sonarr" {
    $payload = $payload | merge { enableSeasonFolders: true }
  }

  let r = http post $"($base_url)/api/v1/settings/($endpoint)" $payload --headers $headers --content-type application/json --full --allow-errors
  if $r.status not-in [200, 201] {
    error make { msg: $"Failed to create ($kind) server ($cfg.name): ($r.status) - ($r.body)" }
  }
  print $"  Created ($kind) server: ($cfg.name)"
}

def set_application_url [app_url: string] {
  print "Setting Application URL..."
  let payload = { applicationUrl: $app_url }
  let r = http post $"($base_url)/api/v1/settings/main" $payload --headers $headers --content-type application/json --full --allow-errors
  if $r.status not-in [200, 204] {
    error make { msg: $"Failed to set Application URL: ($r.status) - ($r.body)" }
  }
  print $"  Application URL set to: ($app_url)"
}

def sync_jellyfin_users [users: record] {
  print "Syncing Jellyfin users..."

  # Get configured usernames from Nix config
  let wanted_usernames = $users | transpose key value | get value | get username
  if ($wanted_usernames | is-empty) {
    print "  No users configured"
    return
  }

  # Get list of Jellyfin users from settings endpoint
  let jf_users = http get $"($base_url)/api/v1/settings/jellyfin/users" --headers $headers --full --allow-errors
  if $jf_users.status != 200 {
    error make { msg: $"Failed to get Jellyfin users: ($jf_users.status) - ($jf_users.body)" }
  }

  # Only import users that are in our Nix config
  let jf_to_import = $jf_users.body | where { |u| ($u.username? | default "") in $wanted_usernames }
  let jf_user_ids = $jf_to_import | get id
  if ($jf_user_ids | is-empty) {
    print "  No matching Jellyfin users to sync"
    return
  }

  let payload = { jellyfinUserIds: $jf_user_ids }
  let r = http post $"($base_url)/api/v1/user/import-from-jellyfin" $payload --headers $headers --content-type application/json --full --allow-errors
  if $r.status not-in [200, 201] {
    error make { msg: $"Failed to sync Jellyfin users: ($r.status) - ($r.body)" }
  }

  let imported = ($r.body | default [])
  if ($imported | length) > 0 {
    print $"  Imported ($imported | length) users from Jellyfin"
  } else {
    print "  All configured users already synced"
  }
}

# Permission bitmask values from Jellyseerr v2.2.3
# Source: https://github.com/Fallenbagel/jellyseerr/blob/v2.2.3/server/lib/permissions.ts
const PERM_REQUEST               = 32
const PERM_REQUEST_4K            = 1024
const PERM_REQUEST_ADVANCED      = 8192
const PERM_AUTO_APPROVE          = 128
const PERM_AUTO_APPROVE_MOVIE    = 256
const PERM_AUTO_APPROVE_TV       = 512
const PERM_AUTO_APPROVE_4K       = 32768
const PERM_AUTO_APPROVE_4K_MOVIE = 65536
const PERM_AUTO_APPROVE_4K_TV    = 131072
const PERM_RECENT_VIEW           = 67108864

# Composite permission sets
const REQUEST_ALL = $PERM_REQUEST bit-or $PERM_REQUEST_4K
const AUTO_APPROVE_ALL = (
  $PERM_AUTO_APPROVE
  bit-or $PERM_AUTO_APPROVE_MOVIE
  bit-or $PERM_AUTO_APPROVE_TV
  bit-or $PERM_AUTO_APPROVE_4K
  bit-or $PERM_AUTO_APPROVE_4K_MOVIE
  bit-or $PERM_AUTO_APPROVE_4K_TV
)

# Mask of all bits managed by this script (used to clear before applying desired state)
const MANAGED_MASK = (
  $REQUEST_ALL
  bit-or $PERM_REQUEST_ADVANCED
  bit-or $AUTO_APPROVE_ALL
  bit-or $PERM_RECENT_VIEW
)

def configure_user_permissions [users: record] {
  print "Configuring user permissions..."

  # Get all Jellyseerr users
  let all_users = http get $"($base_url)/api/v1/user" --headers $headers --full --allow-errors
  if $all_users.status != 200 {
    error make { msg: $"Failed to get users: ($all_users.status) - ($all_users.body)" }
  }

  let users_list = $all_users.body.results? | default []
  for entry in ($users | transpose key value) {
    let username = $entry.value.username
    let user = $users_list | where { |u| ($u.jellyfinUsername? | default "") == $username } | get 0?
    if $user == null {
      error make { msg: $"Failed to find user: ($username)" }
    }

    # Build desired permissions from config
    mut managed_perms = $REQUEST_ALL
    if $entry.value.autoApprove {
      $managed_perms = $managed_perms bit-or $AUTO_APPROVE_ALL
    }
    if $entry.value.advancedRequests {
      $managed_perms = $managed_perms bit-or $PERM_REQUEST_ADVANCED
    }
    if $entry.value.viewRecentlyAdded {
      $managed_perms = $managed_perms bit-or $PERM_RECENT_VIEW
    }

    # Clear managed bits, preserve any other Jellyseerr-granted permissions
    let current_perms = $user.permissions? | default 0
    let base_perms = $current_perms bit-and (0xFFFFFFFF bit-xor $MANAGED_MASK)
    let desired_perms = $base_perms bit-or $managed_perms
    if $current_perms == $desired_perms {
      print $"  User '($username)' permissions already configured"
      continue
    }

    let user_id = $user.id
    let r = http put $"($base_url)/api/v1/user/($user_id)" { permissions: $desired_perms } --headers $headers --content-type application/json --full --allow-errors
    if $r.status not-in [200, 204] {
      error make { msg: $"Failed to update permissions for '($username)': ($r.status) - ($r.body)" }
    }
    print $"  Updated permissions for '($username)'"
  }
}

def main [] {
  wait_ready
  complete_setup_wizard

  set_application_url $config.applicationUrl
  ensure_arr_server "Radarr" $config.radarr $radarr_api_key
  ensure_arr_server "Sonarr" $config.sonarr $sonarr_api_key

  let users = $config.users
  if ($users | is-not-empty) {
    sync_jellyfin_users $users
    configure_user_permissions $users
  } else {
    print "  No users to configure"
  }

  print "Jellyseerr initialization complete"
}
