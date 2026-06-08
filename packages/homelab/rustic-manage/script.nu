#!/usr/bin/env nu
# Rustic backup management: backup and verify.
def require_env [name: string] {
  let val = ($env | get -o $name)
  if ($val == null or ($val | is-empty)) {
    error make {
      msg: $"($name) required"
    }
  } else {
    $val
  }
}
def notify-success [body: string] {
  let send = require_env "SEND_NOTIFICATION"
  ^$send --topic (require_env "NOTIFY_TOPIC") --title "Backup OK" --tags "white_check_mark" --message $body
}
# Run backup, prune old snapshots, and notify. `profile` selects the rustic config (/etc/rustic/<profile>.toml).
def "main backup" [profile: string] {
  let summary_file = $"(require_env 'STATE_DIR')/($profile)-last-summary.txt"
  let check = do { ^rustic -P $profile snapshots --json } | complete
  if $check.exit_code != 0 {
    print "=== Initializing repository ==="
    ^rustic -P $profile init
  }
  print "=== Running rustic backup ==="
  let snapshot = ^rustic -P $profile backup --json | from json
  let s = $snapshot.summary
  let total = $s.total_bytes_processed | into filesize
  let added = $s.data_added_packed | into filesize
  let summary = $"**\(($profile)\) Processed:** ($total) | **Added:** ($added)\n**Files:** ($s.files_new) new, ($s.files_changed) changed"
  $summary | save --force $summary_file
  print "=== Pruning old snapshots ==="
  ^rustic -P $profile forget
  try { notify-success $summary } catch {|e| print $"notify failed: ($e.msg)" }
}
def "main verify" [profile: string] {
  print "=== Checking repository integrity ==="
  ^rustic -P $profile check
  print "=== Checking data integrity (500MB subset) ==="
  ^rustic -P $profile check "--read-data" "--read-data-subset=500MB" # Reasonable sample for homelab-scale backups
}
def main [] { print "rustic-manage - Rustic backup management

  backup <profile>    Run backup, prune, and notify
  verify <profile>    Check repository and data integrity" }
