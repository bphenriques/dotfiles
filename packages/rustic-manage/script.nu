#!/usr/bin/env nu

# Rustic backup management: backup and verify with ntfy notifications.
#
# Required env:
#   RUSTIC_PROFILES    - Colon-separated profile paths (passed via -P)
#   STATE_DIR          - State directory for runtime artifacts
#   NTFY_URL           - ntfy topic URL
#   NTFY_PASSWORD_FILE - Path to file containing ntfy admin password

def require_env [name: string] {
  let val = ($env | get -o $name)
  if ($val == null or ($val | is-empty)) {
    error make { msg: $"($name) required" }
  } else {
    $val
  }
}

def rustic [...args: string] {
  let profiles = require_env "RUSTIC_PROFILES" | split row ':' | each {|p| [-P $p] } | flatten
  ^rustic ...$profiles ...$args
}

def notify [--title: string, --tags: string, --priority: string, body: string] {
  let url = require_env "NTFY_URL"
  let password = open --raw (require_env "NTFY_PASSWORD_FILE") | str trim

  mut headers = { Title: $title, Tags: $tags }
  if $priority != null and ($priority | is-not-empty) {
    $headers = ($headers | insert Priority $priority)
  }

  http post --user admin --password $password --headers $headers --content-type text/plain $url $body
}

def notify-success [body: string] {
  notify --title '✅ Backup OK' --tags white_check_mark $body
}

def notify-failure [body: string] {
  notify --title '❌ Backup Failed' --tags x --priority high $body
}

# Run backup, prune old snapshots, and notify.
def "main backup" [] {
  let summary_file = $"(require_env 'STATE_DIR')/last-summary.txt"

  try {
    print "=== Running rustic backup ==="
    let snapshot = rustic backup --json | from json
    let s = $snapshot.summary

    let total = $s.total_bytes_processed | into filesize
    let added = $s.data_added_packed | into filesize
    let summary = $"Processed: ($total) | Added: ($added)\nFiles: ($s.files_new) new, ($s.files_changed) changed"
    $summary | save --force $summary_file

    print "=== Pruning old snapshots ==="
    rustic forget

    notify-success $summary
  } catch {|err|
    notify-failure $"Backup failed: ($err.msg)"
    error make { msg: $err.msg }
  }
}

# Check repository and data integrity, and notify on failure.
def "main verify" [] {
  try {
    print "=== Checking repository integrity ==="
    rustic check

    print "=== Checking data integrity (500MB subset) ==="
    rustic check --read-data --read-data-subset=500MB
  } catch {|err|
    notify-failure $"Verification failed: ($err.msg)"
    error make { msg: $err.msg }
  }
}

def main [] {
  print "rustic-manage — Rustic backup management

  backup              Run backup, prune, and notify
  verify              Check repository and data integrity"
}
