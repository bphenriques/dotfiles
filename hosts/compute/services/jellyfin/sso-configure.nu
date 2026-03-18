#!/usr/bin/env nu

# Configures the Jellyfin SSO plugin and related branding via its API
# Runs after jellyfin-configure.service

let base_url = $env.JELLYFIN_URL
let admin_username = open $env.JELLYFIN_ADMIN_USERNAME_FILE | str trim
let admin_password = open $env.JELLYFIN_ADMIN_PASSWORD_FILE | str trim
let config = open $env.SSO_CONFIG_FILE
let oidc_client_id = open $env.OIDC_CLIENT_ID_FILE | str trim
let oidc_client_secret = open $env.OIDC_CLIENT_SECRET_FILE | str trim

def wait_ready [] {
  print "Waiting for Jellyfin..."
  for attempt in 1..30 {
    let r = try { http get $"($base_url)/health" --max-time 2sec --full --allow-errors } catch { null }
    if $r != null and $r.status == 200 { return }
    sleep 1sec
  }
  error make { msg: "Jellyfin failed to respond after 30 attempts" }
}

def authenticate []: nothing -> record<headers: list, api_key: string> {
  print $"Authenticating as: ($admin_username)"
  let r = http post $"($base_url)/Users/AuthenticateByName" { Username: $admin_username, Pw: $admin_password } --content-type application/json --headers [Authorization "MediaBrowser Client=\"nix\", Device=\"nix\", DeviceId=\"nix\", Version=\"1\""] --full --allow-errors
  if $r.status != 200 { error make { msg: $"Failed to authenticate: ($r.status) - ($r.body | to json)" } }
  
  let headers = [Authorization $"MediaBrowser Token=\"($r.body.AccessToken)\""]
  
  let existing = http get $"($base_url)/Auth/Keys" --headers $headers --full --allow-errors
  if $existing.status == 200 {
    let matches = $existing.body.Items | where AppName == "jellyfin-sso-configure"
    if not ($matches | is-empty) { return { headers: $headers, api_key: ($matches | first).AccessToken } }
  }
  
  let create = http post $"($base_url)/Auth/Keys?app=jellyfin-sso-configure" { } --headers $headers --content-type application/json --full --allow-errors
  if $create.status != 204 { error make { msg: $"Failed to create API key: ($create.status)" } }
  
  let keys = http get $"($base_url)/Auth/Keys" --headers $headers --full --allow-errors
  let matches = $keys.body.Items | where AppName == "jellyfin-sso-configure"
  if ($matches | is-empty) { error make { msg: "Failed to retrieve created API key" } }
  { headers: $headers, api_key: ($matches | first).AccessToken }
}

def ensure_branding [headers: list] {
  print "Configuring branding..."
  let r = http post $"($base_url)/System/Configuration/Branding" $config.brandingConfig --content-type application/json --headers $headers --full --allow-errors
  if $r.status != 204 { error make { msg: $"Failed to configure branding: ($r.status)" } }
  print "  Branding configured"
}

def ensure_sso [api_key: string] {
  print "Configuring SSO..."
  let provider_name = $config.ssoConfig.providerName
  
  let r = http get $"($base_url)/sso/OID/Get?api_key=($api_key)" --full --allow-errors
  if $r.status == 404 { error make { msg: "SSO plugin not installed" } }
  if $r.status != 200 { error make { msg: $"Failed to query SSO providers: ($r.status)" } }
  
  # OID/Add creates or overwrites the provider config
  let sso_config = $config.ssoConfig | update oidClientId $oidc_client_id | update oidSecret $oidc_client_secret
  let r2 = http post $"($base_url)/sso/OID/Add/($provider_name | url encode)?api_key=($api_key)" $sso_config --content-type application/json --full --allow-errors
  if $r2.status not-in [200, 204] { error make { msg: $"Failed to configure SSO provider ($provider_name): ($r2.status)" } }
  print $"  SSO provider ($provider_name) configured"
}

def main [] {
  wait_ready
  let auth = authenticate
  ensure_branding $auth.headers
  ensure_sso $auth.api_key
  print "SSO configuration complete"
}
