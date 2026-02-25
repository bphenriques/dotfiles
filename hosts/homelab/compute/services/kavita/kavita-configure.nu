#!/usr/bin/env nu

let base_url = $env.KAVITA_URL
let data_dir = $env.KAVITA_DATA_DIR
let config = open $env.KAVITA_CONFIG_FILE
let appsettings_path = $"($data_dir)/config/appsettings.json"

def wait_ready [] {
  for attempt in 1..60 {
    print $"Waiting for Kavita... ($attempt)"
    try { http get $"($base_url)/api/health" --max-time 2sec | ignore; return } catch { sleep 2sec }
  }
  error make { msg: "Kavita failed to start" }
}

def generate_password [file: string] {
  if ($file | path exists) {
    return (open $file | str trim)
  }
  let password = (random chars --length 32)
  $password | save --force $file
  chmod 600 $file
  print $"Generated admin password and saved to ($file)"
  $password
}

def admin_exists [] {
  let r = http get $"($base_url)/api/admin/exists" --full --allow-errors
  if $r.status == 200 {
    $r.body
  } else if $r.status == 404 {
    false
  } else {
    error make { msg: $"Failed to check admin existence: ($r.status) - ($r.body)" }
  }
}

def register_admin [password: string] {
  let body = {
    username: "admin"
    password: $password
    email: ""
  }
  let r = http post $"($base_url)/api/Account/register" $body --content-type application/json --full --allow-errors
  if $r.status == 200 {
    print "Admin user registered successfully"
    $r.body
  } else if $r.status == 400 {
    print "Admin already exists"
    null
  } else {
    error make { msg: $"Failed to register admin: ($r.status) - ($r.body)" }
  }
}

def login [username: string, password: string] {
  let body = { userName: $username, password: $password }
  let r = http post $"($base_url)/api/Account/login" $body --content-type application/json --full --allow-errors
  if $r.status != 200 {
    error make { msg: $"Failed to login: ($r.status) - ($r.body)" }
  }
  $r.body.token
}

def get_settings [headers: record] {
  let r = http get $"($base_url)/api/Settings" --headers $headers --full --allow-errors
  if $r.status != 200 { error make { msg: $"Failed to get settings: ($r.status) - ($r.body)" } }
  $r.body
}

def update_settings [settings: record, headers: record] {
  let r = http post $"($base_url)/api/Settings" $settings --headers $headers --content-type application/json --full --allow-errors
  if $r.status != 200 { error make { msg: $"Failed to update settings: ($r.status) - ($r.body)" } }
  print "Server settings updated"
}

def get_libraries [headers: record] {
  let r = http get $"($base_url)/api/Library/libraries" --headers $headers --full --allow-errors
  if $r.status != 200 { error make { msg: $"Failed to get libraries: ($r.status) - ($r.body)" } }
  $r.body
}

def create_library [lib: record, headers: record] {
  let body = {
    name: $lib.name
    type: $lib.type
    folders: $lib.folders
    folderWatching: true
    includeInDashboard: true
    includeInRecommended: true
    includeInSearch: true
    manageCollections: true
    manageReadingLists: true
    allowScrobbling: false
    enableMetadata: true
  }
  let r = http post $"($base_url)/api/Library/create" $body --headers $headers --content-type application/json --full --allow-errors
  if $r.status == 200 {
    print $"Created library: ($lib.name)"
    true
  } else {
    print $"Warning: Failed to create library ($lib.name): ($r.status) - ($r.body)"
    false
  }
}

def ensure_libraries [libraries: list, headers: record] {
  let existing = get_libraries $headers
  let existing_names = $existing | get name

  $libraries | each { |lib|
    if $lib.name in $existing_names {
      print $"Library ($lib.name) already exists"
    } else {
      create_library $lib $headers
    }
  } | ignore
}

def update_oidc_settings [oidc_id: string, oidc_secret: string, library_ids: list, headers: record] {
  let current_settings = get_settings $headers
  let oidc = $config.oidc

  let updated_settings = $current_settings
    | upsert oidcAuthority $oidc.authority
    | upsert oidcClientId $oidc_id
    | upsert oidcClientSecret $oidc_secret
    | upsert oidcProviderName $oidc.buttonText
    | upsert oidcProvisionAccounts $oidc.provisionAccounts
    | upsert oidcSyncUserSettings $oidc.syncUserSettings
    | upsert oidcRolesClaim $oidc.rolesClaim
    | upsert oidcRolesPrefix $oidc.rolesPrefix
    | upsert oidcDefaultRoles $oidc.defaultRoles
    | upsert oidcDefaultLibraries $library_ids

  update_settings $updated_settings $headers
}

def update_appsettings [oidc_id: string, oidc_secret: string] {
  if not ($appsettings_path | path exists) {
    print "appsettings.json not found, skipping"
    return
  }

  let current = open $appsettings_path
  let authentication = {
    Authority: $config.oidc.authority
    ClientId: $oidc_id
    ClientSecret: $oidc_secret
  }

  let needs_update = (
    ($current | get -i Authentication.Authority | default "") != $authentication.Authority or
    ($current | get -i Authentication.ClientId | default "") != $authentication.ClientId or
    ($current | get -i Authentication.ClientSecret | default "") != $authentication.ClientSecret
  )

  if $needs_update {
    let updated = $current | upsert Authentication $authentication
    $updated | save --force $appsettings_path
    print "Updated appsettings.json with OIDC credentials"
    print "NOTE: Kavita requires a restart for OIDC changes to take effect on first setup"
  } else {
    print "appsettings.json OIDC configuration is up to date"
  }
}

def main [] {
  wait_ready
  print "Kavita is ready"

  let oidc_id = open $"($env.CREDENTIALS_DIRECTORY)/oidc-id" | str trim
  let oidc_secret = open $"($env.CREDENTIALS_DIRECTORY)/oidc-secret" | str trim

  # Update appsettings.json with OIDC credentials (needed for startup)
  update_appsettings $oidc_id $oidc_secret

  # Generate admin password and register if needed
  let password = generate_password $config.adminPasswordFile
  if not (admin_exists) {
    register_admin $password
  }

  # Login and configure via API
  let token = login "admin" $password
  let headers = { "Authorization": $"Bearer ($token)" }

  # Create libraries
  ensure_libraries $config.libraries $headers

  # Get library IDs for OIDC default libraries
  let libraries = get_libraries $headers
  let library_ids = $libraries | get id

  # Update OIDC settings via API
  update_oidc_settings $oidc_id $oidc_secret $library_ids $headers

  print "Kavita configuration complete"
}
