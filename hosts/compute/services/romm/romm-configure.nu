#!/usr/bin/env nu

let base_url = $env.ROMM_URL
let admin_username = $env.ROMM_ADMIN_USERNAME
let admin_email = $env.ROMM_ADMIN_EMAIL
let admin_password_file = $env.ROMM_ADMIN_PASSWORD_FILE

def wait_ready [] {
  for attempt in 1..60 {
    print $"Waiting for RomM... ($attempt)"
    try { http get $"($base_url)/api/heartbeat" --max-time 2sec | ignore; return } catch { sleep 2sec }
  }
  error make { msg: "RomM failed to start" }
}

def ensure_password [file: string]: nothing -> string {
  if ($file | path exists) {
    return (open $file | str trim)
  }
  let password = random chars --length 32
  $password | save --force $file
  chmod 600 $file
  print $"Generated admin password: ($file)"
  $password
}

def create_admin_user [password: string] {
  let cookies_file = $"/tmp/romm_cookies_(random chars --length 8)"

  let heartbeat = http get $"($base_url)/api/heartbeat" --headers [Accept "application/json"] --full --allow-errors
  if $heartbeat.status != 200 {
    error make { msg: $"Heartbeat failed: ($heartbeat.status)" }
  }

  ^curl -s -c $cookies_file $"($base_url)/api/heartbeat" -o /dev/null
  let csrf_token = open $cookies_file | lines | where { |l| $l | str contains "romm_csrftoken" } | first | split row "\t" | last

  let body = {
    username: $admin_username
    password: $password
    email: $admin_email
    role: "admin"
  }

  let r = ^curl -s -X POST $"($base_url)/api/users" -b $cookies_file -H "Content-Type: application/json" -H $"X-CSRFToken: ($csrf_token)" -d ($body | to json -r) -w "\n%{http_code}"
  let lines = $r | lines
  let status_code = $lines | last | into int
  let response_body = $lines | drop 1 | str join "\n"

  rm -f $cookies_file

  if $status_code == 201 or $status_code == 200 {
    print $"Admin user '($admin_username)' created successfully"
  } else if $status_code == 400 and ($response_body | str contains "already exists") {
    print $"Admin user '($admin_username)' already exists"
  } else if $status_code == 400 and ($response_body | str contains "username") {
    print $"Admin user '($admin_username)' already exists"
  } else {
    error make { msg: $"Failed to create admin user: ($status_code) - ($response_body)" }
  }
}

def main [] {
  let password = ensure_password $admin_password_file
  wait_ready
  print "RomM is ready"
  create_admin_user $password
  print "RomM initialization complete"
}
