#!/usr/bin/env nu
let base_url = $env.KAVITA_URL
let config = open $env.KAVITA_CONFIG_FILE
def wait_ready [] {
  for attempt in 1..60 {
    print $"Waiting for Kavita... ($attempt)"
    try {
      http get $"($base_url)/api/health" --max-time 2sec | ignore
      return
    } catch { sleep 2sec }
  }
  error make {msg: "Kavita failed to start after 60 attempts"}
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
    error make {
      msg: $"Failed to register admin: ($r.status) - ($r.body)"
    }
  }
}
def login [username: string, password: string] {
  let body = {
    userName: $username
    password: $password
  }
  let r = http post $"($base_url)/api/Account/login" $body --content-type application/json --full --allow-errors
  if $r.status != 200 {
    error make {
      msg: $"Failed to login: ($r.status) - ($r.body)"
    }
  }
  $r.body.token
}
def get_settings [headers: record] {
  let r = http get $"($base_url)/api/Settings" --headers $headers --full --allow-errors
  if $r.status != 200 {
    error make {
      msg: $"Failed to get settings: ($r.status) - ($r.body)"
    }
  }
  $r.body
}
def update_settings [settings: record, headers: record] {
  let r = http post $"($base_url)/api/Settings" $settings --headers $headers --content-type application/json --full --allow-errors
  if $r.status != 200 {
    error make {
      msg: $"Failed to update settings: ($r.status) - ($r.body)"
    }
  }
}
def get_libraries [headers: record] {
  let r = http get $"($base_url)/api/Library/libraries" --headers $headers --full --allow-errors
  if $r.status != 200 {
    error make {
      msg: $"Failed to get libraries: ($r.status) - ($r.body)"
    }
  }
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
    error make {
      msg: $"Failed to create library ($lib.name): ($r.status) - ($r.body)"
    }
  }
}
def update_library [existing: record, lib: record, headers: record] {
  let body = {
    id: $existing.id
    name: $lib.name
    type: $lib.type
    folders: $lib.folders
    fileGroupTypes: $lib.fileGroupTypes
    folderWatching: ($existing.folderWatching? | default true)
    includeInDashboard: ($existing.includeInDashboard? | default true)
    includeInSearch: ($existing.includeInSearch? | default true)
    manageCollections: ($existing.manageCollections? | default true)
    manageReadingLists: ($existing.manageReadingLists? | default true)
    allowScrobbling: ($existing.allowScrobbling? | default false)
    allowMetadataMatching: ($existing.allowMetadataMatching? | default true)
    enableMetadata: ($existing.enableMetadata? | default true)
    removePrefixForSortName: ($existing.removePrefixForSortName? | default false)
    inheritWebLinksFromFirstChapter: ($existing.inheritWebLinksFromFirstChapter? | default false)
    excludePatterns: ($existing.excludePatterns? | default [])
  }
  let r = http post $"($base_url)/api/Library/update" $body --headers $headers --content-type application/json --full --allow-errors
  if $r.status != 200 {
    error make {
      msg: $"Failed to update library ($lib.name): ($r.status) - ($r.body)"
    }
  }
  print $"Updated library: ($lib.name)"
}
def ensure_libraries [libraries: list<any>, headers: record] {
  let existing = get_libraries $headers
  $libraries | each { |lib|
    let found = $existing | where name == $lib.name | get 0?
    if $found == null {
      create_library $lib $headers
    } else {
      update_library $found $lib $headers
    }
  } | ignore
}
def update_server_settings [headers: record] {
  let current_settings = get_settings $headers
  let server = $config.server
  let updated_settings = $current_settings | upsert hostName $server.hostName | upsert allowStatCollection $server.allowStatCollection | upsert enableFolderWatching $server.enableFolderWatching
  update_settings $updated_settings $headers
  print "Server settings updated"
}
def update_oidc_settings [library_map: record, headers: record] {
  let current_settings = get_settings $headers
  let oidc = $config.oidc
  # Resolve library names to IDs
  let default_library_ids = $oidc.defaultLibraries | each { |name| $library_map | get $name }
  # OIDC for authentication only - roles managed via API
  let base_oidc_config = ($current_settings.oidcConfig? | default {})
  let updated_oidc_config = $base_oidc_config | upsert providerName $oidc.buttonText | upsert provisionAccounts $oidc.provisionAccounts | upsert syncUserSettings $oidc.syncUserSettings | upsert defaultRoles $oidc.defaultRoles | upsert defaultLibraries $default_library_ids | upsert defaultAgeRestriction $oidc.defaultAgeRestriction | upsert defaultIncludeUnknowns $oidc.defaultIncludeUnknowns | upsert autoLogin $oidc.autoLogin | upsert disablePasswordAuthentication $oidc.disablePasswordAuth
  let updated_settings = $current_settings | upsert oidcConfig $updated_oidc_config
  update_settings $updated_settings $headers
  print "OIDC settings updated"
}
def get_users [headers: record] {
  let r = http get $"($base_url)/api/Users" --headers $headers --full --allow-errors
  if $r.status != 200 {
    error make {
      msg: $"Failed to get users: ($r.status) - ($r.body)"
    }
  }
  $r.body
}
def update_user [
  kavita_user: record
  roles: list<any>
  library_ids: list<any>
  age_restriction: record
  headers: record
] {
  let body = {
    userId: $kavita_user.id
    username: $kavita_user.username
    email: ($kavita_user.email? | default "")
    roles: $roles
    libraries: $library_ids
    ageRestriction: $age_restriction
    identityProvider: $kavita_user.identityProvider
  }
  let r = http post $"($base_url)/api/Account/update" $body --headers $headers --content-type application/json --full --allow-errors
  if $r.status != 200 {
    error make {
      msg: $"Failed to update user ($kavita_user.username): ($r.status) - ($r.body)"
    }
  }
}
def reconcile_oidc_users [library_map: record, headers: record] {
  let oidc = $config.oidc
  let admin_usernames = ($config.oidcAdminUsernames? | default [])
  let expected_library_ids = $oidc.defaultLibraries | each { |name| $library_map | get $name } | sort
  let default_roles = $oidc.defaultRoles
  let expected_age = {
    ageRating: $oidc.defaultAgeRestriction
    includeUnknowns: $oidc.defaultIncludeUnknowns
  }
  let users = get_users $headers
  # identityProvider: 0=Kavita, 1=OIDC
  let oidc_users = $users | where identityProvider == 1
  for user in $oidc_users {
    let expected_roles = if ($user.username in $admin_usernames) { ["Admin"] } else { $default_roles }
    let current_library_ids = $user.libraries | get id | sort
    let current_roles = $user.roles | sort
    let current_age = $user.ageRestriction
    let libs_match = $current_library_ids == $expected_library_ids
    let roles_match = $current_roles == ($expected_roles | sort)
    let age_match = ($current_age.ageRating == $expected_age.ageRating) and ($current_age.includeUnknowns == $expected_age.includeUnknowns)
    if $libs_match and $roles_match and $age_match {
      print $"  OIDC user '($user.username)' already up to date"
    } else {
      update_user $user $expected_roles $expected_library_ids $expected_age $headers
      print $"  Updated OIDC user '($user.username)': roles=($expected_roles), libraries=($oidc.defaultLibraries), ageRestriction=($expected_age)"
    }
  }
}
def provision_local_users [library_map: record, headers: record] {
  if ($config.localUsers? | default [] | is-empty) {
    print "  No local users configured"
    return
  }
  for user in $config.localUsers {
    let password = open ($env.CREDENTIALS_DIRECTORY | path join $user.passwordCredential) | str trim
    # Register user if not yet created (idempotent - 400 means already exists)
    let existing_users = get_users $headers
    let found = $existing_users | where username == $user.username | get 0?
    if $found == null {
      let body = {
        username: $user.username
        password: $password
        email: ($user.email? | default "")
      }
      let r = http post $"($base_url)/api/Account/register" $body --content-type application/json --full --allow-errors
      if $r.status == 200 {
        print $"  Created local user: ($user.username)"
      } else if $r.status != 400 {
        error make {
          msg: $"Failed to register local user ($user.username): ($r.status) - ($r.body)"
        }
      }
    } else {
      print $"  Local user '($user.username)' already exists"
    }
    # Ensure roles and library access (re-fetch to get the user record after possible creation)
    let kavita_users = get_users $headers
    let kavita_user = $kavita_users | where username == $user.username | get 0?
    if $kavita_user != null {
      let library_ids = $user.libraries | each { |name| $library_map | get $name }
      let age_restriction = {ageRating: -1, includeUnknowns: true}
      update_user $kavita_user $user.roles $library_ids $age_restriction $headers
      print $"  Updated '($user.username)': roles=($user.roles), libraries=($user.libraries)"
    }
  }
}
def main [] {
  wait_ready
  print "Kavita is ready"
  # Generate admin password and register if needed (idempotent - 400 means already exists)
  let password = open $config.adminPasswordFile | str trim
  register_admin $password
  # Login and configure via API
  let token = login $config.adminUsername $password
  let headers = {
    "Authorization": $"Bearer ($token)"
  }
  # Create libraries
  ensure_libraries $config.libraries $headers
  # Build library name -> ID map for settings and user sync
  let libraries = get_libraries $headers
  let library_map = $libraries | each { |lib| [$lib.name $lib.id] } | into record
  # Update settings via API
  update_server_settings $headers
  update_oidc_settings $library_map $headers
  # Reconcile OIDC users: ensure they have public libraries
  # (defaultLibraries only applies at first provisioning, not retroactively)
  print "Reconciling OIDC users..."
  reconcile_oidc_users $library_map $headers
  # Provision local users (e.g., guest accounts)
  print "Provisioning local users..."
  provision_local_users $library_map $headers
  print "Kavita configuration complete"
}
