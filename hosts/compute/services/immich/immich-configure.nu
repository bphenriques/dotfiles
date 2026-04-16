#!/usr/bin/env nu

let base_url = $env.IMMICH_URL
let config = open $env.IMMICH_CONFIG_FILE

def wait_ready [] {
  for attempt in 1..60 {
    print $"Waiting for Immich... ($attempt)"
    try { http get $"($base_url)/api/server/ping" --max-time 2sec | ignore; return } catch { sleep 2sec }
  }
  error make { msg: "Immich failed to start after 60 attempts" }
}

def admin_signup [admin: record, password: string] {
  let body = {
    email: $admin.email
    name: $admin.name
    password: $password
  }
  let r = http post $"($base_url)/api/auth/admin-sign-up" $body --content-type application/json --full --allow-errors
  if $r.status == 201 {
    print $"Admin user ($admin.email) registered successfully"
  } else if $r.status == 400 and ($r.body | to text | str contains "admin") {
    print "Admin already exists, skipping signup"
  } else {
    error make { msg: $"Failed to register admin: ($r.status) - ($r.body)" }
  }
}

def complete_admin_onboarding [headers: record] {
  # System-level admin onboarding
  let r = http post $"($base_url)/api/system-metadata/admin-onboarding" {} --headers $headers --content-type application/json --full --allow-errors
  if $r.status == 204 or $r.status == 200 {
    print "System admin onboarding completed"
  } else if $r.status == 400 {
    print "System admin onboarding already completed"
  } else {
    print $"Warning: Failed to complete system admin onboarding: ($r.status)"
  }
}

def login [email: string, password: string] {
  let body = { email: $email, password: $password }
  let r = http post $"($base_url)/api/auth/login" $body --content-type application/json --full --allow-errors
  if $r.status != 201 {
    error make { msg: $"Failed to login: ($r.status) - ($r.body)" }
  }
  $r.body.accessToken
}

def get_all [endpoint: string, headers: record] {
  let r = http get $"($base_url)/api($endpoint)" --headers $headers --full --allow-errors
  if $r.status != 200 { error make { msg: $"Failed to get ($endpoint): ($r.status) - ($r.body)" } }
  $r.body
}

def ensure_user [user: record, existing: list, headers: record] {
  let is_admin = ($user.isAdmin? | default false)
  let found = $existing | where email == $user.email | get 0?
  if $found != null {
    if ($found.isAdmin != $is_admin) {
      let body = { isAdmin: $is_admin }
      let r = http put $"($base_url)/api/admin/users/($found.id)" $body --headers $headers --content-type application/json --full --allow-errors
      if $r.status != 200 { error make { msg: $"Failed to update user ($user.email): ($r.status) - ($r.body)" } }
      print $"User ($user.email) admin status updated to ($is_admin)"
    } else {
      print $"User ($user.email) already exists"
    }
    return $found.id
  }

  let body = {
    email: $user.email
    name: $user.name
    password: (random chars --length 32)
    shouldChangePassword: false
  }
  let r = http post $"($base_url)/api/admin/users" $body --headers $headers --content-type application/json --full --allow-errors
  if $r.status != 201 { error make { msg: $"Failed to create user ($user.email): ($r.status) - ($r.body)" } }
  print $"Created user ($user.email)"
  $r.body.id
}

def ensure_library [lib: record, owner_id: string, existing: list, headers: record] {
  let found = $existing | where name == $lib.name and ownerId == $owner_id | get 0?

  if $found != null {
    if ($found.importPaths | sort) != ($lib.importPaths | sort) {
      let body = {
        name: $lib.name
        importPaths: $lib.importPaths
        exclusionPatterns: $lib.exclusionPatterns
      }
      let r = http put $"($base_url)/api/libraries/($found.id)" $body --headers $headers --content-type application/json --full --allow-errors
      if $r.status != 200 { error make { msg: $"Failed to update library ($lib.name): ($r.status) - ($r.body)" } }
      print $"Updated library ($lib.name) import paths"
    } else {
      print $"Library ($lib.name) already exists with correct paths"
    }
    return $found.id
  }

  let body = {
    name: $lib.name
    ownerId: $owner_id
    importPaths: $lib.importPaths
    exclusionPatterns: $lib.exclusionPatterns
  }
  let r = http post $"($base_url)/api/libraries" $body --headers $headers --content-type application/json --full --allow-errors
  if $r.status != 201 { error make { msg: $"Failed to create library ($lib.name): ($r.status) - ($r.body)" } }
  print $"Created library ($lib.name)"
  let scan = http post $"($base_url)/api/libraries/($r.body.id)/scan" "{}" --headers $headers --content-type application/json --full --allow-errors
  if $scan.status == 204 {
    print $"Triggered scan for library ($lib.name)"
  } else {
    print $"Warning: Failed to trigger scan for library ($lib.name): ($scan.status)"
  }
  $r.body.id
}

def main [] {
  wait_ready
  print "Immich is ready"

  let admin = $config.admin
  let password = open $admin.passwordFile | str trim

  # Register admin on first run
  admin_signup $admin $password

  # Login and use access token for API operations
  let token = login $admin.email $password
  let headers = { "Authorization": $"Bearer ($token)" }

  # Complete admin onboarding to skip the getting started wizard
  complete_admin_onboarding $headers

  # Create users and complete their onboarding
  let existing_users = get_all "/admin/users" $headers
  let user_map = $config.users
    | each { |u|
        let user_id = ensure_user $u $existing_users $headers
        [$u.email $user_id]
      }
    | into record

  # Create libraries
  let existing_libraries = get_all "/libraries" $headers
  $config.libraries | each { |lib|
    ensure_library $lib ($user_map | get $lib.ownerEmail) $existing_libraries $headers
  } | ignore

  print "Immich initialization complete"
}
