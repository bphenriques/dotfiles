#!/usr/bin/env nu

let base_url = $env.KAVITA_URL
let config = open $env.KAVITA_CONFIG_FILE

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
    fileGroupTypes: $lib.fileGroupTypes
    excludePatterns: []
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

def update_server_settings [headers: record] {
  let current_settings = get_settings $headers
  let server = $config.server

  let updated_settings = $current_settings
    | upsert hostName $server.hostName
    | upsert allowStatCollection $server.allowStatCollection
    | upsert enableFolderWatching $server.enableFolderWatching

  update_settings $updated_settings $headers
  print "Server settings updated"
}

def update_oidc_settings [library_ids: list, headers: record] {
  let current_settings = get_settings $headers
  let oidc = $config.oidc

  # Update the nested oidcConfig object with our settings
  # Note: authority, clientId, secret are read from appsettings.json by Kavita
  let updated_oidc_config = $current_settings.oidcConfig
    | upsert providerName $oidc.buttonText
    | upsert provisionAccounts $oidc.provisionAccounts
    | upsert syncUserSettings $oidc.syncUserSettings
    | upsert rolesClaim $oidc.rolesClaim
    | upsert rolesPrefix $oidc.rolesPrefix
    | upsert defaultRoles $oidc.defaultRoles
    | upsert defaultLibraries $library_ids
    | upsert autoLogin $oidc.autoLogin
    | upsert disablePasswordAuthentication $oidc.disablePasswordAuth

  let updated_settings = $current_settings | upsert oidcConfig $updated_oidc_config

  update_settings $updated_settings $headers
  print "OIDC settings updated"
}

def main [] {
  wait_ready
  print "Kavita is ready"

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

  # Update settings via API
  update_server_settings $headers
  update_oidc_settings $library_ids $headers

  print "Kavita configuration complete"
}
