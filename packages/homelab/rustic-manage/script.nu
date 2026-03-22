#!/usr/bin/env nu

# Rustic backup management: backup and verify.

def require_env [name: string] {
  let val = ($env | get -o $name)
  if ($val == null or ($val | is-empty)) {
    error make { msg: $"($name) required" }
  } else {
    $val
  }
}

def notify [--title: string, --tags: string, --priority: string, body: string] {
  let url = require_env "NTFY_URL"
  let token = open --raw (require_env "NTFY_TOKEN_FILE") | str trim

  mut headers = { Title: $title, Tags: $tags, Authorization: $"Bearer ($token)", Markdown: "yes" }
  if $priority != null and ($priority | is-not-empty) {
    $headers = ($headers | insert Priority $priority)
  }

  http post --headers $headers --content-type text/plain $url $body
}

def notify-success [body: string] {
  notify --title 'Backup OK' --tags white_check_mark $body
}

# Run backup, prune old snapshots, and notify.
def "main backup" [] {
  let summary_file = $"(require_env 'STATE_DIR')/last-summary.txt"

  let check = do { ^rustic snapshots --json } | complete
  if $check.exit_code != 0 {
    print "=== Initializing repository ==="
    ^rustic init
  }

  print "=== Running rustic backup ==="
  let snapshot = ^rustic backup --json | from json
  let s = $snapshot.summary

  let total = $s.total_bytes_processed | into filesize
  let added = $s.data_added_packed | into filesize
  let summary = $"**Processed:** ($total) | **Added:** ($added)\n**Files:** ($s.files_new) new, ($s.files_changed) changed"
  $summary | save --force $summary_file

  print "=== Pruning old snapshots ==="
  ^rustic forget

  try { notify-success $summary } catch {|e| print $"notify failed: ($e.msg)" }
}

# Check repository and data integrity.
def "main verify" [] {
  print "=== Checking repository integrity ==="
  ^rustic check

  print "=== Checking data integrity (500MB subset) ==="
  ^rustic check "--read-data" "--read-data-subset=500MB"
}

def main [] {
  print "rustic-manage - Rustic backup management

  backup              Run backup, prune, and notify
  verify              Check repository and data integrity"
}
