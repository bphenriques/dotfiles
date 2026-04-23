#!/usr/bin/env nu
# Configures Gitea: creates admin user and sets up OIDC authentication source.
let base_url = $env.GITEA_URL
let config_flag = $"-c=($env.GITEA_CONFIG)"
let admin_password = open $env.GITEA_ADMIN_PASSWORD_FILE | str trim
let source_name = $env.OIDC_PROVIDER_NAME
let client_id = open $env.OIDC_CLIENT_ID_FILE | str trim
let client_secret = open $env.OIDC_CLIENT_SECRET_FILE | str trim
def wait_ready [] {
  for attempt in 1..60 {
    print $"Waiting for Gitea... ($attempt)"
    try {
      http get $"($base_url)/api/healthz" --max-time 2sec | ignore
      return
    } catch { sleep 2sec }
  }
  error make {msg: "Gitea failed to start after 60 attempts"}
}
def ensure_admin [] {
  let r = ^gitea $config_flag admin user list --admin | complete
  if $r.exit_code != 0 or ($r.stdout | str trim | lines | length) <= 1 {
    ^gitea $config_flag admin user create --admin --username admin --password $admin_password --email "admin@localhost" --must-change-password=false
    print "Admin user created"
  } else {
    print "Admin user already exists"
  }
}
def find_auth_source_id [name: string] {
  let r = ^gitea $config_flag admin auth list --vertical-bars | complete
  if $r.exit_code != 0 { return null }
  $r.stdout | str trim | lines | skip 1 | each { |line|
    let cols = $line | split row "|" | each { |c| $c | str trim }
    if ($cols | length) >= 4 and $cols.1 == $name {
      $cols.0 | into int
    }
  } | flatten | get 0?
}
def ensure_oidc_source [] {
  let oauth_args = [
    --name
    $source_name
    --provider
    openidConnect
    --key
    $client_id
    --secret
    $client_secret
    --auto-discover-url
    $env.OIDC_DISCOVERY_URL
    --scopes
    "openid,email,profile,groups"
  ]
  let existing_id = find_auth_source_id $source_name
  if $existing_id != null {
    ^gitea $config_flag admin auth update-oauth --id ($existing_id | into string) ...$oauth_args
    print $"OIDC source '($source_name)' updated"
    return
  }
  # Try to create; if it already exists (race or name mismatch in list parsing), find and update instead
  let r = ^gitea $config_flag admin auth add-oauth ...$oauth_args | complete
  if $r.exit_code == 0 {
    print $"OIDC source '($source_name)' created"
    return
  }
  if ($r.stderr | str contains "already exists") {
    let id = find_auth_source_id $source_name
    if $id != null {
      ^gitea $config_flag admin auth update-oauth --id ($id | into string) ...$oauth_args
      print $"OIDC source '($source_name)' updated"
      return
    }
  }
  error make {
    msg: $"Failed to configure OIDC source: ($r.stderr)"
  }
}
def main [] {
  wait_ready
  print "Gitea is ready"
  ensure_admin
  ensure_oidc_source
  print "Gitea configuration complete"
}
