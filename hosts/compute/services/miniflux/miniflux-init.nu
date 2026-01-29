#!/usr/bin/env nu

# Configures Miniflux users declaratively via the API.
# Creates users with openid_connect_id from Pocket ID, then applies settings.

let base_url = $env.MINIFLUX_URL
let admin_username = open $env.MINIFLUX_ADMIN_USERNAME_FILE | str trim
let admin_password = open $env.MINIFLUX_ADMIN_PASSWORD_FILE | str trim
let user_settings = open $env.MINIFLUX_USER_SETTINGS_FILE
let oidc_users = open $env.OIDC_USERS_FILE

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

def create_user [username: string, is_admin: bool, openid_connect_id: string] {
  let body = {
    username: $username
    password: (random chars --length 32)
    openid_connect_id: $openid_connect_id
    is_admin: $is_admin
  } | to json

  let r = http post $"($base_url)/v1/users" $body --user $admin_username --password $admin_password --content-type "application/json" --full --allow-errors
  if $r.status != 201 { error make { msg: $"Failed to create user ($username): ($r.status) - ($r.body)" } }
  $r.body
}

def update_user [user_id: int, settings: record] {
  let r = http put $"($base_url)/v1/users/($user_id)" ($settings | to json) --user $admin_username --password $admin_password --content-type "application/json" --full --allow-errors
  if $r.status != 201 { error make { msg: $"Failed to update user ($user_id): ($r.status) - ($r.body)" } }
}

def main [] {
  print "miniflux-init: starting"
  wait_ready

  if ($user_settings | is-empty) {
    print "miniflux-init: no user settings configured"
    return
  }

  print $"miniflux-init: processing ($user_settings | length) user\(s\)"
  let miniflux_users = get_users

  $user_settings | each { |cfg|
    let oidc_user = $oidc_users | where username == $cfg.username | get 0?
    if $oidc_user == null {
      print $"  ($cfg.username): not found in OIDC users, skipping"
      return
    }

    let openid_connect_id = $oidc_user.id
    let miniflux_user = $miniflux_users | where username == $cfg.username | get 0?

    let user_id = if $miniflux_user == null {
      let created = create_user $cfg.username $cfg.is_admin $openid_connect_id
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
  } | ignore

  print "miniflux-init: done"
}
