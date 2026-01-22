#!/usr/bin/env nu

# Reports users and groups in Pocket ID that are not managed by Nix.
# Usage: pocket-id-drift.nu
const PAGINATION_LIMIT = 20

let base_url = $env.POCKET_ID_URL
let api_key = open $env.POCKET_ID_API_KEY_FILE | str trim
let config = open $env.POCKET_ID_CONFIG_FILE | from json

def api [method: string, endpoint: string, body?: record] {
  let url = $"($base_url)/api($endpoint)"
  let headers = { "X-API-KEY": $api_key, "Content-Type": "application/json" }
  if $body != null {
    http $method $url $body --headers $headers --full --allow-errors
  } else {
    http $method $url --headers $headers --full --allow-errors
  }
}

def get_all [resource: string] {
  let r = api "get" $"/($resource)?pagination[limit]=($PAGINATION_LIMIT)"
  if $r.status != 200 { error make { msg: $"Failed to list ($resource): ($r.status) - ($r.body)" } }
  $r.body.data
}

def is_managed [entity: record] -> bool {
  $entity.customClaims? | default [] | any { |c| $c.key == "managed-by" and $c.value == "nix" }
}

def report_drift [label: string, items: list, name_field: string, extra_field?: string] {
  if ($items | is-empty) { return }
  print $label
  $items | sort-by $name_field | each { |item|
    let name = $item | get $name_field
    if $extra_field != null {
      print $"  - ($name) <($item | get $extra_field)>"
    } else {
      print $"  - ($name)"
    }
  }
  print ""
}

def main [] {
  print "Checking for drift between Nix configuration and Pocket ID..."
  print ""

  let desired_users = $config.users | get username
  let desired_groups = $config.groups | get name

  let actual_users = get_all "users"
  let unmanaged_users = $actual_users | where { |u| $u.username not-in $desired_users }
  report_drift "⚠️  Users managed by Nix but removed from config:" ($unmanaged_users | where { |u| is_managed $u }) "username" "email"
  report_drift "ℹ️  Manually created users (not managed by Nix):" ($unmanaged_users | where { |u| not (is_managed $u) }) "username" "email"

  let actual_groups = get_all "user-groups"
  let unmanaged_groups = $actual_groups | where { |g| $g.name not-in $desired_groups }
  report_drift "⚠️  Groups managed by Nix but removed from config:" ($unmanaged_groups | where { |g| is_managed $g }) "name"
  report_drift "ℹ️  Manually created groups (not managed by Nix):" ($unmanaged_groups | where { |g| not (is_managed $g) }) "name"

  if ($unmanaged_users | is-empty) and ($unmanaged_groups | is-empty) {
    print "✅ No drift detected. Pocket ID state matches Nix configuration."
  }
}
