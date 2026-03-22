#!/usr/bin/env nu

# Generates Radicale htpasswd from homelab user secrets.

def main [] {
  let config = open $env.RADICALE_PROVISION_FILE
  let users = $config | get -o users | default {}
  let htpasswd_file = $config.htpasswdFile

  if ($users | is-empty) {
    print "No users configured, creating empty htpasswd"
    "" | save --force $htpasswd_file
    ^chmod "640" $htpasswd_file
    ^chown "radicale:radicale" $htpasswd_file
    return
  }

  let tmp = $"($htpasswd_file).tmp"

  print $"Generating htpasswd for ($users | transpose | length) users..."
  mut first = true
  for entry in ($users | transpose name user) {
    let password = open --raw $entry.user.passwordFile | str trim
    let create_flag = if $first { ["-c"] } else { [] }
    $password | ^htpasswd -iB ...$create_flag $tmp $entry.name
    print $"  ($entry.name): ok"
    $first = false
  }

  ^chmod "640" $tmp
  ^chown "radicale:radicale" $tmp
  ^mv -f $tmp $htpasswd_file

  print "Radicale htpasswd generation complete"
}
