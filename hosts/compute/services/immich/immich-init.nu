#!/usr/bin/env nu

let base_url = $env.IMMICH_URL
let api_key = open $env.IMMICH_API_KEY_FILE | str trim
let users = $env.IMMICH_USERS_JSON | from json
let libraries = $env.IMMICH_LIBRARIES_JSON | from json

def api [method: string, endpoint: string, body?: record] {
  let url = $"($base_url)/api($endpoint)"
  let headers = { "x-api-key": $api_key, "Content-Type": "application/json" }
  if $body != null {
    http $method $url $body --headers $headers --full --allow-errors
  } else {
    http $method $url --headers $headers --full --allow-errors
  }
}

def wait_ready [] {
  for _ in 1..60 {
    try {
      let r = api "get" "/server/ping"
      if $r.status == 200 { return }
    } catch { }
    sleep 2sec
  }
  error make { msg: "Immich failed to start" }
}

def get_all [endpoint: string] {
  let r = api "get" $endpoint
  if $r.status != 200 { error make { msg: $"Failed to get ($endpoint): ($r.status)" } }
  $r.body
}

def ensure_user [user: record, existing: list] {
  let found = $existing | where email == $user.email | first --skip-empty
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
  let r = api "post" "/admin/users" $body
  if $r.status != 201 { error make { msg: $"Failed to create user ($user.email): ($r.status) - ($r.body)" } }
  print $"Created user ($user.email)"
  $r.body.id
}

def ensure_library [lib: record, owner_id: string, existing: list] {
  let found = $existing | where { |l| $l.name == $lib.name and $l.ownerId == $owner_id } | first --skip-empty

  if $found != null {
    if ($found.importPaths | sort) != ($lib.importPaths | sort) {
      let r = api "put" $"/libraries/($found.id)" {
        name: $lib.name
        importPaths: $lib.importPaths
        exclusionPatterns: $lib.exclusionPatterns
      }
      if $r.status != 200 { error make { msg: $"Failed to update library ($lib.name): ($r.status)" } }
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
  let r = api "post" "/libraries" $body
  if $r.status != 201 { error make { msg: $"Failed to create library ($lib.name): ($r.status) - ($r.body)" } }
  print $"Created library ($lib.name)"
  api "post" $"/libraries/($r.body.id)/scan" {} | ignore
  print $"Triggered scan for library ($lib.name)"
  $r.body.id
}

def main [] {
  wait_ready

  let existing_users = get_all "/admin/users"
  let user_map = $users
    | each { |u| { email: $u.email, id: (ensure_user $u $existing_users) } }
    | transpose -rd

  let existing_libraries = get_all "/libraries"
  $libraries | each { |lib|
    ensure_library $lib ($user_map | get $lib.ownerEmail) $existing_libraries
  } | ignore
}
