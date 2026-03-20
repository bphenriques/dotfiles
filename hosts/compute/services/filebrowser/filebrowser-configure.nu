#!/usr/bin/env nu

# Configures FileBrowser proxy auth and per-user scopes via the CLI.
#
# Environment variables:
#   FILEBROWSER_CONFIG_FILE - Path to JSON config file
#   FILEBROWSER_DB          - Path to FileBrowser database
#
# Config file format:
# {
#   "defaults": {
#     "scope": "/shared",
#     "permissions": { "create": true, "delete": false, ... }
#   },
#   "users": [
#     { "username": "bphenriques", "scope": "/", "admin": true }
#   ]
# }

let config = open $env.FILEBROWSER_CONFIG_FILE

def fb [...args] {
  let r = ^filebrowser -d $env.FILEBROWSER_DB ...$args | complete
  if $r.exit_code != 0 {
    error make { msg: $"filebrowser failed: ($args | str join ' ')\n($r.stderr)" }
  }
  $r.stdout
}

def fb_try [...args] {
  ^filebrowser -d $env.FILEBROWSER_DB ...$args | complete
}

def configure_defaults [] {
  let d = $config.defaults
  let p = $d.permissions

  let args = [
    $"--root=($env.FILEBROWSER_ROOT)"
    "--auth.method=proxy"
    "--auth.header=Remote-User" # From Tinyauth
    $"--scope=($d.scope)"
    $"--perm.create=($p.create)"
    $"--perm.delete=($p.delete)"
    $"--perm.rename=($p.rename)"
    $"--perm.modify=($p.modify)"
    $"--perm.execute=($p.execute)"
    $"--perm.share=($p.share)"
    $"--perm.download=($p.download)"
    "--perm.admin=false"
    "--hideLoginButton"
    $"--branding.name=($config.branding.name)"
    $"--branding.disableExternal=($config.branding.disableExternal)"
    $"--branding.disableUsedPercentage=($config.branding.disableUsedPercentage)"
    $"--viewMode=($config.viewMode)"
    $"--singleClick=($config.singleClick)"
    $"--hideDotfiles=($config.hideDotfiles)"
    $"--sorting.by=($config.sorting.by)"
    $"--sorting.asc=($config.sorting.asc)"
  ]

  fb config set ...$args
  print "Defaults configured"
}

def ensure_user [user: record] {
  let exists = (fb_try users find $user.username).exit_code == 0

  # Use per-user permissions if set, otherwise inherit defaults
  let p = $user.permissions? | default $config.defaults.permissions

  let user_args = [
    $"--scope=($user.scope)"
    $"--perm.admin=($user.admin)"
    $"--perm.create=($p.create)"
    $"--perm.delete=($p.delete)"
    $"--perm.rename=($p.rename)"
    $"--perm.modify=($p.modify)"
    $"--perm.execute=($p.execute)"
    $"--perm.share=($p.share)"
    $"--perm.download=($p.download)"
    $"--hideDotfiles=($config.hideDotfiles)"
    $"--singleClick=($config.singleClick)"
    $"--viewMode=($config.viewMode)"
    $"--sorting.by=($config.sorting.by)"
    $"--sorting.asc=($config.sorting.asc)"
  ]

  if $exists {
    fb users update $user.username ...$user_args
    print $"  ($user.username): updated"
  } else {
    let password = random chars --length 32
    fb users add $user.username $password ...$user_args
    print $"  ($user.username): created"
  }
}

def init_db [] {
  if not ($env.FILEBROWSER_DB | path exists) {
    fb config init
    print "Database initialized"
  }
}

def main [] {
  init_db
  configure_defaults

  print "Configuring users..."
  $config.users | each { |user| ensure_user $user } | ignore

  print "FileBrowser configuration complete"
}
