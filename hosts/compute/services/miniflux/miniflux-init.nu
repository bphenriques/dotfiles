#!/usr/bin/env nu

# Configures Miniflux user settings declaratively via the API.

let base_url = $env.MINIFLUX_URL
let admin_username = open $env.MINIFLUX_ADMIN_USERNAME_FILE | str trim
let admin_password = open $env.MINIFLUX_ADMIN_PASSWORD_FILE | str trim
let user_settings = open $env.MINIFLUX_USER_SETTINGS_FILE

def wait_ready [] {
  print "Waiting for Miniflux..."
  for attempt in 1..30 {
    try {
      let r = http get $"($base_url)/healthcheck" --max-time 2sec --full --allow-errors
      if $r.status == 200 {
        print "Miniflux is ready"
        return
      }
      print $"Attempt ($attempt): status ($r.status)"
    } catch {
      print $"Attempt ($attempt): connection failed"
    }
    sleep 1sec
  }
  error make { msg: "Miniflux failed to start after 30 attempts" }
}

def get_users [] {
  let r = http get $"($base_url)/v1/users" --user $admin_username --password $admin_password --full --allow-errors
  if $r.status != 200 { error make { msg: $"Failed to list users: ($r.status) - ($r.body)" } }
  $r.body
}

def update_user [user_id: int, settings: record] {
  let r = http put $"($base_url)/v1/users/($user_id)" ($settings | to json) --user $admin_username --password $admin_password --content-type "application/json" --full --allow-errors
  if $r.status != 200 { error make { msg: $"Failed to update user ($user_id): ($r.status) - ($r.body)" } }
}

def main [] {
  print "miniflux-init: starting"
  wait_ready

  if ($user_settings | is-empty) {
    print "miniflux-init: no user settings configured"
    return
  }

  print $"miniflux-init: applying settings for ($user_settings | length) user\(s\)"
  let users = get_users

  $user_settings | each { |cfg|
    let user = $users | where username == $cfg.username | get 0?
    if $user == null {
      print $"  ($cfg.username): not found, skipping"
    } else {
      let settings = $cfg | reject username
      update_user $user.id $settings
      print $"  ($cfg.username): updated"
    }
  } | ignore

  print "miniflux-init: done"
}
