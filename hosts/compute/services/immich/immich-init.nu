#!/usr/bin/env nu

let base_url = $env.IMMICH_URL
let api_key = open $env.IMMICH_API_KEY_FILE | str trim
let users = $env.IMMICH_USERS_JSON | from json
let libraries = $env.IMMICH_LIBRARIES_JSON | from json

def api [method: string, endpoint: string, body?: record] {
  let url = $"($base_url)/api($endpoint)"
  let headers = { "x-api-key": $api_key, "Content-Type": "application/json" }
  if $body != null {
    http $method $url --headers $headers --body $body --full --allow-errors
  } else {
    http $method $url --headers $headers --full --allow-errors
  }
}

def wait_ready [] {
  for i in 1..60 {
    try { 
      let r = api "get" "/server/ping"
      if $r.status == 200 { return }
    } catch { }
    sleep 2sec
  }
  error make { msg: "Immich failed to start" }
}

def get_all_users [] {
  let r = api "get" "/admin/users"
  if $r.status != 200 { error make { msg: $"Failed to list users: ($r.status)" } }
  $r.body
}

def get_all_libraries [] {
  let r = api "get" "/libraries"
  if $r.status != 200 { error make { msg: $"Failed to list libraries: ($r.status)" } }
  $r.body
}

def ensure_user [user: record] {
  let existing = get_all_users | where email == $user.email | first --skip-empty
  if $existing != null { 
    print $"User ($user.email) already exists"
    return $existing.id 
  }
  
  let body = {
    email: $user.email
    name: $user.name
    password: (random chars --length 32)
    shouldChangePassword: false
  }
  
  let r = api "post" "/admin/users" $body
  if $r.status == 201 { 
    print $"Created user ($user.email)"
    return $r.body.id 
  }
  error make { msg: $"Failed to create user ($user.email): ($r.status) - ($r.body)" }
}

def ensure_library [lib: record, owner_id: string] {
  let existing = get_all_libraries | where { |l| $l.name == $lib.name and $l.ownerId == $owner_id } | first --skip-empty
  
  if $existing != null {
    # Update import paths if needed
    let current_paths = $existing.importPaths | sort
    let desired_paths = $lib.importPaths | sort
    if $current_paths != $desired_paths {
      let r = api "put" $"/libraries/($existing.id)" { 
        name: $lib.name
        importPaths: $lib.importPaths 
        exclusionPatterns: $lib.exclusionPatterns
      }
      if $r.status != 200 { error make { msg: $"Failed to update library ($lib.name): ($r.status)" } }
      print $"Updated library ($lib.name) import paths"
    } else {
      print $"Library ($lib.name) already exists with correct paths"
    }
    return $existing.id
  }
  
  let body = {
    name: $lib.name
    ownerId: $owner_id
    importPaths: $lib.importPaths
    exclusionPatterns: $lib.exclusionPatterns
  }
  
  let r = api "post" "/libraries" $body
  if $r.status == 201 { 
    print $"Created library ($lib.name)"
    # Trigger initial scan
    api "post" $"/libraries/($r.body.id)/scan" {} | ignore
    print $"Triggered scan for library ($lib.name)"
    return $r.body.id 
  }
  error make { msg: $"Failed to create library ($lib.name): ($r.status) - ($r.body)" }
}

wait_ready

# Create users and build email->id map
let user_map = $users | each { |u|
  let id = ensure_user $u
  { email: $u.email, id: $id }
} | reduce --fold {} { |it, acc| $acc | insert $it.email $it.id }

# Create libraries and assign to owners
$libraries | each { |lib|
  let owner_id = $user_map | get $lib.ownerEmail
  ensure_library $lib $owner_id
} | ignore
