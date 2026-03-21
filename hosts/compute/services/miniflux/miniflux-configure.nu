#!/usr/bin/env nu

# Configures Miniflux users declaratively via the API.
# Creates users with openid_connect_id from Pocket ID, then applies settings.

let base_url = $env.MINIFLUX_URL
let admin_username = open $env.MINIFLUX_ADMIN_USERNAME_FILE | str trim
let admin_password = open $env.MINIFLUX_ADMIN_PASSWORD_FILE | str trim
let user_settings = open $env.MINIFLUX_USER_SETTINGS_FILE
let oidc_users = open $env.OIDC_USERS_FILE

def wait_ready [] {
  for attempt in 1..30 {
    print $"Waiting for Miniflux... ($attempt)"
    try { http get $"($base_url)/healthcheck" --max-time 2sec | ignore; return } catch { sleep 2sec }
  }
  error make { msg: "Miniflux failed to start after 30 attempts" }
}

def get_users [] {
  let r = http get $"($base_url)/v1/users" --user $admin_username --password $admin_password --full --allow-errors
  if $r.status != 200 { error make { msg: $"Failed to list users: ($r.status) - ($r.body)" } }
  $r.body
}

def create_user [username: string, openid_connect_id: string] {
  let body = {
    username: $username
    password: (random chars --length 32)
    openid_connect_id: $openid_connect_id
    is_admin: false # I am not special, there is only one admin that should be used sparingly.
  }

  let r = http post $"($base_url)/v1/users" $body --user $admin_username --password $admin_password --content-type application/json --full --allow-errors
  if $r.status != 201 { error make { msg: $"Failed to create user ($username): ($r.status) - ($r.body)" } }
  $r.body
}

def update_user [user_id: int, settings: record] {
  let r = http put $"($base_url)/v1/users/($user_id)" $settings --user $admin_username --password $admin_password --content-type application/json --full --allow-errors
  if $r.status != 201 { error make { msg: $"Failed to update user ($user_id): ($r.status) - ($r.body)" } }
}

def main [] {
  wait_ready
  print "Miniflux is ready"

  if ($user_settings | default [] | is-empty) {
    print "No user settings configured"
    return
  }

  print $"Processing ($user_settings | length) users"
  let miniflux_users = get_users

  $user_settings | each { |cfg|
    let oidc_user = $oidc_users | where username == $cfg.username | get 0?
    if $oidc_user == null {
      print $"  ($cfg.username): not found in OIDC users, skipping"
    } else {
      let openid_connect_id = $oidc_user.id
      let miniflux_user = $miniflux_users | where username == $cfg.username | get 0?

      let user_id = if $miniflux_user == null {
        let created = create_user $cfg.username $openid_connect_id
        print $"  ($cfg.username): created with openid_connect_id"
        $created.id
      } else {
        $miniflux_user.id
      }

      let settings = $cfg | reject username
      if not ($settings | is-empty) {
        update_user $user_id $settings
        print $"  ($cfg.username): settings updated"
      } else {
        print $"  ($cfg.username): no settings to apply"
      }
    }
  } | ignore

  print "Miniflux initialization complete"
}
