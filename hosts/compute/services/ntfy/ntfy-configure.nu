#!/usr/bin/env nu

# ntfy setup: admin user, topic ACLs, and publisher tokens.
#
# Expects ntfy, chown, chmod in PATH.
#
# Environment variables:
#   NTFY_ADMIN_PASSWORD_FILE - Path to admin password file
#   NTFY_PROVISION_FILE      - Path to JSON config file
#
# Config file format:
# {
#   "publicTopics": ["media"],
#   "publishers": {
#     "radarr":       { "topic": "media",    "tokenFile": "/path", "owner": "radarr" },
#     "transmission": { "topic": "download", "tokenFile": "/path", "owner": "transmission" }
#   }
# }

let config = open $env.NTFY_PROVISION_FILE

def setup_admin [] {
  print "Setting up admin user..."
  let password = open $env.NTFY_ADMIN_PASSWORD_FILE | str trim
  with-env { NTFY_PASSWORD: $password } {
    ^ntfy user add --role=admin --ignore-exists admin
    ^ntfy user change-pass admin
  }
}

def setup_public_topics [] {
  let topics = $config | get -o publicTopics | default []
  if ($topics | is-empty) { return }

  print "Setting ACLs for public topics..."
  for topic in $topics {
    ^ntfy access everyone $topic ro
    print $"  ($topic) → everyone ro"
  }
}

def setup_publishers [] {
  let publishers = $config | get -o publishers | default {}
  if ($publishers | is-empty) { return }

  print "Provisioning publishers..."
  for entry in ($publishers | transpose name pub) {
    let name = $entry.name
    let pub = $entry.pub

    let random_pass = (random chars --length 32)
    with-env { NTFY_PASSWORD: $random_pass } {
      ^ntfy user add --ignore-exists $name
    }
    ^ntfy access $name $pub.topic wo

    if ($pub.tokenFile | path exists) {
      print $"  ($name) → ($pub.topic) \(token exists\)"
    } else {
      let token = (^ntfy token add --label $name $name | str trim | split row " " | get 1)
      $token | save --raw $pub.tokenFile
      ^chown $"($pub.owner):root" $pub.tokenFile
      ^chmod "400" $pub.tokenFile
      print $"  ($name) → ($pub.topic) \(token created\)"
    }
  }
}

# Does not detect whenever a user gets deleted.

def main [] {
  setup_admin
  setup_public_topics
  setup_publishers
  print "ntfy setup complete"
}
