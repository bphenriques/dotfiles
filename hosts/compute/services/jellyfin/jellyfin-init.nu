#!/usr/bin/env nu

# Initializes Jellyfin declaratively via the API:
# - Completes the startup wizard with admin credentials
# - Configures branding (custom CSS, login disclaimer with SSO button)
# - Configures trickplay with hardware acceleration
# - Creates libraries (Movies, TV Shows, Music)
# - Configures SSO plugin for the configured OIDC provider
# - Creates users linked to their OIDC IDs
#
# Limitations:
# - Libraries are additive only: manually created libraries are not removed
# - Users are additive only: manually created users are not removed
# - SSO provider is additive only: existing providers are not removed
# - Full reconciliation would require tracking desired vs actual state and diffing,
#   which adds complexity without significant benefit for a homelab setup

let base_url = $env.JELLYFIN_URL
let admin_username = open $env.JELLYFIN_ADMIN_USERNAME_FILE | str trim
let admin_password = open $env.JELLYFIN_ADMIN_PASSWORD_FILE | str trim
let config = open $env.JELLYFIN_CONFIG_FILE
let oidc_client_id = open $env.OIDC_CLIENT_ID_FILE | str trim
let oidc_client_secret = open $env.OIDC_CLIENT_SECRET_FILE | str trim
let oidc_users = open $env.OIDC_USERS_FILE

def wait_ready [] {
  print "Waiting for Jellyfin..."
  for attempt in 1..30 {
    let r = try { http get $"($base_url)/Startup/Configuration" --max-time 2sec --full --allow-errors } catch { null }
    if $r != null and $r.status in [200, 401, 403] { return }
    sleep 1sec
  }
  error make { msg: "Jellyfin failed to start after 30 attempts" }
}

def is_startup_wizard_complete [] {
  (http get $"($base_url)/Startup/Configuration" --full --allow-errors).status in [401, 403]
}

# Jellyfin Startup Wizard API flow (undocumented, discovered via source/build logs):
#   1. POST /Startup/Configuration - Set locale/metadata preferences
#   2. GET  /Startup/User          - Triggers internal user creation (returns default username)
#   3. POST /Startup/User          - Update the created user with desired credentials
#   4. POST /Startup/RemoteAccess  - Configure remote access settings
#   5. POST /Startup/Complete      - Mark wizard as complete
# Note: Calling POST /Startup/User without GET first returns 500 (no user exists yet)
def complete_startup_wizard [] {
  print "Running startup wizard..."
  
  let r1 = http post $"($base_url)/Startup/Configuration" { UICulture: "en-US", MetadataCountryCode: "US", PreferredMetadataLanguage: "en" } --content-type application/json --full --allow-errors
  if $r1.status != 204 { error make { msg: $"Failed to configure startup: ($r1.status) - ($r1.body)" } }
  
  print $"  Creating admin user: ($admin_username)"
  let r2a = http get $"($base_url)/Startup/User" --full --allow-errors
  if $r2a.status != 200 { error make { msg: $"Failed to get startup user: ($r2a.status) - ($r2a.body)" } }
  
  let r2b = http post $"($base_url)/Startup/User" { Name: $admin_username, Password: $admin_password } --content-type application/json --full --allow-errors
  if $r2b.status != 204 { error make { msg: $"Failed to update admin user: ($r2b.status) - ($r2b.body)" } }
  
  let r3 = http post $"($base_url)/Startup/RemoteAccess" { EnableRemoteAccess: true, EnableAutomaticPortMapping: false } --content-type application/json --full --allow-errors
  if $r3.status != 204 { error make { msg: $"Failed to configure remote access: ($r3.status) - ($r3.body)" } }
  
  let r4 = http post $"($base_url)/Startup/Complete" { } --content-type application/json --full --allow-errors
  if $r4.status != 204 { error make { msg: $"Failed to complete startup: ($r4.status) - ($r4.body)" } }
  
  print "  Startup wizard completed"
}

def authenticate []: nothing -> string {
  print $"Authenticating as: ($admin_username)"
  let r = http post $"($base_url)/Users/AuthenticateByName" { Username: $admin_username, Pw: $admin_password } --content-type application/json --headers [Authorization "MediaBrowser Client=\"nix\", Device=\"nix\", DeviceId=\"nix\", Version=\"1\""] --full --allow-errors
  if $r.status != 200 { error make { msg: $"Failed to authenticate: ($r.status) - ($r.body | to json)" } }
  $r.body.AccessToken
}

def get_api_key [token: string]: nothing -> string {
  let headers = [Authorization $"MediaBrowser Token=\"($token)\""]
  
  let existing = http get $"($base_url)/Auth/Keys" --headers $headers --full --allow-errors
  if $existing.status == 200 {
    let key = $existing.body.Items | where AppName == "jellyfin-init" | first
    if $key != null { return $key.AccessToken }
  }
  
  let r = http post $"($base_url)/Auth/Keys?app=jellyfin-init" { } --headers $headers --content-type application/json --full --allow-errors
  if $r.status != 204 { error make { msg: $"Failed to create API key: ($r.status)" } }
  
  let keys = http get $"($base_url)/Auth/Keys" --headers $headers --full --allow-errors
  let key = $keys.body.Items | where AppName == "jellyfin-init" | first
  if $key == null { error make { msg: "Failed to retrieve created API key" } }
  $key.AccessToken
}

def ensure_server_name [headers: list] {
  print "Configuring server name..."
  let current = http get $"($base_url)/System/Configuration" --headers $headers --full --allow-errors
  if $current.status != 200 { error make { msg: $"Failed to get system config: ($current.status)" } }
  
  let updated = $current.body | update ServerName $config.serverName
  let r = http post $"($base_url)/System/Configuration" $updated --content-type application/json --headers $headers --full --allow-errors
  if $r.status != 204 { error make { msg: $"Failed to configure server name: ($r.status)" } }
  print $"  Server name set to: ($config.serverName)"
}

def ensure_branding [headers: list] {
  print "Configuring branding..."
  let r = http post $"($base_url)/System/Configuration/Branding" $config.brandingConfig --content-type application/json --headers $headers --full --allow-errors
  if $r.status != 204 { error make { msg: $"Failed to configure branding: ($r.status)" } }
  print "  Branding configured"
}

def ensure_trickplay [headers: list] {
  print "Configuring trickplay..."
  let current = http get $"($base_url)/System/Configuration/encoding" --headers $headers --full --allow-errors
  if $current.status != 200 { error make { msg: $"Failed to get encoding config: ($current.status)" } }
  
  let r = http post $"($base_url)/System/Configuration/encoding" ($current.body | merge $config.trickplayConfig) --content-type application/json --headers $headers --full --allow-errors
  if $r.status != 204 { error make { msg: $"Failed to configure trickplay: ($r.status)" } }
  print "  Trickplay configured"
}

def ensure_libraries [headers: list] {
  print "Configuring libraries..."
  let existing = http get $"($base_url)/Library/VirtualFolders" --headers $headers --full --allow-errors
  if $existing.status != 200 { error make { msg: $"Failed to get libraries: ($existing.status)" } }
  let existing_names = $existing.body | get Name
  
  for lib in $config.libraries {
    if $lib.Name in $existing_names {
      print $"  Library exists: ($lib.Name)"
    } else {
      let paths = $lib.Locations | each { |p| $"&paths=($p | url encode)" } | str join ""
      let url = $"($base_url)/Library/VirtualFolders?name=($lib.Name | url encode)&collectionType=($lib.CollectionType)($paths)&refreshLibrary=false"
      let body = {
        LibraryOptions: {
          EnableRealtimeMonitor: $lib.EnableRealtimeMonitor
          ExtractTrickplayImagesDuringLibraryScan: $lib.ExtractTrickplayImagesDuringLibraryScan
        }
      }
      let r = http post $url $body --content-type application/json --headers $headers --full --allow-errors
      if $r.status != 204 { error make { msg: $"Failed to create library ($lib.Name): ($r.status)" } }
      print $"  Created library: ($lib.Name)"
    }
  }
}

def ensure_sso [api_key: string] {
  print "Configuring SSO..."
  let provider_name = $config.ssoProviderName
  
  let r = http get $"($base_url)/sso/OID/Get?api_key=($api_key)" --full --allow-errors
  if $r.status == 404 { error make { msg: "SSO plugin not installed" } }
  if $r.status != 200 { error make { msg: $"Failed to query SSO providers: ($r.status)" } }
  
  if ($r.body | default {} | get -o $provider_name) != null {
    print $"  SSO provider ($provider_name) already configured"
    return
  }
  
  let sso_config = $config.ssoConfig | update oidClientId $oidc_client_id | update oidSecret $oidc_client_secret
  let create = http post $"($base_url)/sso/OID/Add/($provider_name | url encode)?api_key=($api_key)" $sso_config --content-type application/json --full --allow-errors
  if $create.status not-in [200, 204] { error make { msg: $"Failed to configure SSO provider ($provider_name): ($create.status)" } }
  print $"  SSO provider ($provider_name) configured"
}

def ensure_users [headers: list] {
  print "Configuring users..."
  let user_configs = $config.userConfigs
  if ($user_configs | is-empty) {
    print "  No users configured"
    return
  }
  
  let existing = http get $"($base_url)/Users" --headers $headers --full --allow-errors
  if $existing.status != 200 { error make { msg: $"Failed to get users: ($existing.status)" } }
  
  for cfg in $user_configs {
    let in_oidc = ($oidc_users | where username == $cfg.username | first) != null
    if not $in_oidc {
      error make { msg: $"User ($cfg.username) not found in OIDC users" }
    }
    
    let existing_user = $existing.body | where Name == $cfg.username | first
    if $existing_user == null {
      let r = http post $"($base_url)/Users/New" { Name: $cfg.username, Password: (random chars --length 32) } --content-type application/json --headers $headers --full --allow-errors
      if $r.status != 200 { error make { msg: $"Failed to create user ($cfg.username): ($r.status)" } }
      print $"  ($cfg.username): created"
    } else {
      print $"  ($cfg.username): exists"
    }
    
    # Update user policy if configured
    if ($cfg.policy? | is-not-empty) {
      let users = http get $"($base_url)/Users" --headers $headers --full --allow-errors
      let user = $users.body | where Name == $cfg.username | first
      let updated_policy = $user.Policy | merge $cfg.policy
      let r = http post $"($base_url)/Users/($user.Id)/Policy" $updated_policy --content-type application/json --headers $headers --full --allow-errors
      if $r.status != 204 { error make { msg: $"Failed to update policy for ($cfg.username): ($r.status)" } }
      print $"  ($cfg.username): policy updated"
    }
  }
}

def main [] {
  wait_ready
  print "Jellyfin is ready"

  if (is_startup_wizard_complete) { print "Startup wizard already completed" } else {
    complete_startup_wizard
    sleep 2sec
  }

  let token = authenticate
  let api_key = get_api_key $token
  let headers = [Authorization $"MediaBrowser Token=\"($token)\""]
  
  ensure_server_name $headers
  ensure_branding $headers
  ensure_trickplay $headers
  ensure_libraries $headers
  ensure_sso $api_key
  ensure_users $headers

  print "Jellyfin initialization complete"
}
