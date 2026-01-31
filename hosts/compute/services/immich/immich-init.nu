#!/usr/bin/env nu

# Initializes Immich: users and external libraries.

let base_url = $env.IMMICH_URL
let api_key = open $env.IMMICH_API_KEY_FILE | str trim
let users = $env.IMMICH_USERS_JSON | from json
let libraries = $env.IMMICH_LIBRARIES_JSON | from json
let headers = { "x-api-key": $api_key }

def wait_ready [] {
  for attempt in 1..60 {
    print $"Waiting for Immich... ($attempt)"
    try { http get $"($base_url)/api/server/ping" --headers $headers --max-time 2sec | ignore; return } catch { sleep 2sec }
  }
  error make { msg: "Immich failed to start" }
}

def get_all [endpoint: string] {
  let r = http get $"($base_url)/api($endpoint)" --headers $headers --full --allow-errors
  if $r.status != 200 { error make { msg: $"Failed to get ($endpoint): ($r.status) - ($r.body)" } }
  $r.body
}

def ensure_user [user: record, existing: list] {
  let found = $existing | where email == $user.email | get 0?
  if $found != null {
    print $"User ($user.email) already exists"
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

def ensure_library [lib: record, owner_id: string, existing: list] {
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

  let existing_users = get_all "/admin/users"
  let user_map = $users
    | each { |u| [$u.email (ensure_user $u $existing_users)] }
    | into record

  let existing_libraries = get_all "/libraries"
  $libraries | each { |lib|
    ensure_library $lib ($user_map | get $lib.ownerEmail) $existing_libraries
  } | ignore

  print "Immich initialization complete"
}
