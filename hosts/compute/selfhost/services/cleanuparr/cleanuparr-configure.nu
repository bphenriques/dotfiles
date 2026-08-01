#!/usr/bin/env nu
# Idempotent Cleanuparr reconcile. Config lives in a database behind an authenticated API, so this
# bootstraps an account, then applies each section read-modify-write to preserve keys we do not declare.
let base_url = $env.CLEANUPARR_URL
let config = open $env.CLEANUPARR_CONFIG_FILE

def creds [] {
  let raw = open $env.CLEANUPARR_CREDENTIALS_FILE | str trim
  {username: "provisioner", password: $raw}
}

def wait_ready [] {
  for attempt in 1..60 {
    print $"Waiting for Cleanuparr... ($attempt)"
    let r = try { http get $"($base_url)/api/auth/status" --full --allow-errors } catch { null }
    if $r != null and $r.status == 200 { return $r.body }
    sleep 2sec
  }
  error make {msg: "Cleanuparr failed to start after 60 attempts"}
}

# First run only. Changing the password in the UI breaks login until the secret is regenerated.
def ensure_account [status: record] {
  if ($status | get -o setupCompleted | default false) {
    return
  }
  print "Bootstrapping the provisioning account..."
  let c = creds
  let r = http post $"($base_url)/api/auth/setup/account" $c --content-type application/json --full --allow-errors
  if $r.status not-in [200, 201] { error make {msg: $"Failed to create account: ($r.status) - ($r.body)"} }
  let done = http post $"($base_url)/api/auth/setup/complete" {} --content-type application/json --full --allow-errors
  if $done.status not-in [200, 201, 204] { error make {msg: $"Failed to complete setup: ($done.status) - ($done.body)"} }
}

def login [] {
  let r = http post $"($base_url)/api/auth/login" (creds) --content-type application/json --full --allow-errors
  if $r.status != 200 { error make {msg: $"Login failed: ($r.status) - ($r.body)"} }
  if ($r.body | get -o requiresTwoFactor | default false) {
    error make {msg: "The provisioning account has 2FA enabled, so the reconcile cannot log in"}
  }
  $r.body.tokens.accessToken
}

def get_config [token: string, section: string] {
  let r = http get $"($base_url)/api/configuration/($section)" --headers [Authorization $"Bearer ($token)"] --full --allow-errors
  if $r.status != 200 { error make {msg: $"GET ($section) failed: ($r.status) - ($r.body)"} }
  $r.body
}

def put_config [token: string, section: string, payload: record] {
  let r = http put $"($base_url)/api/configuration/($section)" $payload --headers [Authorization $"Bearer ($token)"] --content-type application/json --full --allow-errors
  if $r.status not-in [200, 202, 204] { error make {msg: $"PUT ($section) failed: ($r.status) - ($r.body)"} }
  print $"  Applied ($section)"
}

# Nested records merge one level deep, which covers every section here.
def overlay [current: record, changes: record] {
  $changes | transpose key value | reduce --fold $current { |it, acc|
    let existing = $acc | get -o $it.key
    let merged = if ($existing | describe | str starts-with "record") and ($it.value | describe | str starts-with "record") {
      $existing | merge $it.value
    } else {
      $it.value
    }
    $acc | upsert $it.key $merged
  }
}

def apply_section [token: string, section: string, changes: record] {
  let current = get_config $token $section
  let desired = overlay $current $changes
  if $desired == $current {
    print $"  ($section) already matches"
    return
  }
  put_config $token $section $desired
}

# Instances are a list keyed by name: update the one we manage, leave anything else alone.
def ensure_arr_instance [token: string, arr: string, instance: record] {
  let current = get_config $token $arr
  let existing = $current.instances | where name == $instance.name | get 0?
  let payload = {
    enabled: true
    name: $instance.name
    url: $instance.url
    apiKey: (open $instance.apiKeyFile | str trim)
    version: $instance.version
  }
  let headers = [Authorization $"Bearer ($token)"]
  if $existing == null {
    let r = http post $"($base_url)/api/configuration/($arr)/instances" $payload --headers $headers --content-type application/json --full --allow-errors
    if $r.status not-in [200, 201] { error make {msg: $"Failed to create ($arr) instance: ($r.status) - ($r.body)"} }
    print $"  Created ($arr) instance: ($instance.name)"
  } else {
    let r = http put $"($base_url)/api/configuration/($arr)/instances/($existing.id)" $payload --headers $headers --content-type application/json --full --allow-errors
    if $r.status not-in [200, 202, 204] { error make {msg: $"Failed to update ($arr) instance: ($r.status) - ($r.body)"} }
    print $"  Updated ($arr) instance: ($instance.name)"
  }
}

def ensure_download_client [token: string, client: record] {
  let current = get_config $token "download_client"
  let existing = $current.clients | where name == $client.name | get 0?
  let headers = [Authorization $"Bearer ($token)"]
  if $existing == null {
    let r = http post $"($base_url)/api/configuration/download_client" $client --headers $headers --content-type application/json --full --allow-errors
    if $r.status not-in [200, 201] { error make {msg: $"Failed to create download client: ($r.status) - ($r.body)"} }
    print $"  Created download client: ($client.name)"
  } else {
    let r = http put $"($base_url)/api/configuration/download_client/($existing.id)" ($client | merge {id: $existing.id}) --headers $headers --content-type application/json --full --allow-errors
    if $r.status not-in [200, 202, 204] { error make {msg: $"Failed to update download client: ($r.status) - ($r.body)"} }
    print $"  Updated download client: ($client.name)"
  }
}

# Their own resource: sent inline on queue_cleaner they are silently dropped by the update DTO.
def ensure_stall_rule [token: string] {
  let rule = $config | get -o stallRule
  if $rule == null { return }
  let headers = [Authorization $"Bearer ($token)"]
  let existing = http get $"($base_url)/api/queue-rules/stall" --headers $headers --full --allow-errors
  if $existing.status != 200 { error make {msg: $"Failed to list stall rules: ($existing.status) - ($existing.body)"} }
  let found = $existing.body | default [] | where name == $rule.name | get 0?
  if $found == null {
    let r = http post $"($base_url)/api/queue-rules/stall" $rule --headers $headers --content-type application/json --full --allow-errors
    if $r.status not-in [200, 201] { error make {msg: $"Failed to create stall rule: ($r.status) - ($r.body)"} }
    print $"  Created stall rule: ($rule.name)"
  } else {
    let r = http put $"($base_url)/api/queue-rules/stall/($found.id)" ($rule | merge {id: $found.id}) --headers $headers --content-type application/json --full --allow-errors
    if $r.status not-in [200, 202, 204] { error make {msg: $"Failed to update stall rule: ($r.status) - ($r.body)"} }
    print $"  Updated stall rule: ($rule.name)"
  }
}

def ensure_notification [token: string] {
  let n = $config | get -o notification
  if $n == null { return }
  let existing = http get $"($base_url)/api/configuration/notification_providers" --headers [Authorization $"Bearer ($token)"] --full --allow-errors
  if $existing.status != 200 { error make {msg: $"Failed to list notification providers: ($existing.status)"} }
  let found = $existing.body.providers | default [] | where name == "ntfy" | get 0?
  let payload = {
    name: "ntfy"
    isEnabled: true
    serverUrl: $n.serverUrl
    topics: [$n.topic]
    authenticationType: "AccessToken"
    accessToken: (open $env.NTFY_TOKEN_FILE | str trim)
    priority: "Default"
    tags: [$n.tags]
    onFailedImportStrike: true
    onStalledStrike: true
    onQueueItemDeleted: true
    onDownloadCleaned: true
  }
  # Upsert, not create-once: the event flags above must still reach an instance that already has it.
  let headers = [Authorization $"Bearer ($token)"]
  if $found == null {
    let r = http post $"($base_url)/api/configuration/notification_providers/ntfy" $payload --headers $headers --content-type application/json --full --allow-errors
    if $r.status not-in [200, 201] { error make {msg: $"Failed to create notification: ($r.status) - ($r.body)"} }
    print "  Created notification: ntfy"
  } else {
    let r = http put $"($base_url)/api/configuration/notification_providers/ntfy/($found.id)" $payload --headers $headers --content-type application/json --full --allow-errors
    if $r.status not-in [200, 202, 204] { error make {msg: $"Failed to update notification: ($r.status) - ($r.body)"} }
    print "  Updated notification: ntfy"
  }
}

def main [] {
  let status = wait_ready
  print "Cleanuparr is ready"
  ensure_account $status
  let token = login

  apply_section $token "general" {
    dryRun: $config.dryRun
    displaySupportBanner: false
    statusCheckEnabled: false  # no outbound version pings
  }

  ensure_download_client $token $config.downloadClient
  for arr in ["sonarr" "radarr"] {
    let instance = $config | get -o $arr
    if $instance != null {
      ensure_arr_instance $token $arr $instance
      # Too low and a transient failure costs a re-download.
      apply_section $token $arr {failedImportMaxStrikes: $config.queueCleaner.failedImportMaxStrikes}
    }
  }

  apply_section $token "queue_cleaner" {
    enabled: true
    failedImport: {
      maxStrikes: $config.queueCleaner.failedImportMaxStrikes
      # Include mode with no patterns is rejected; Exclude means "strike everything except these".
      patternMode: "Exclude"
      skipIfNotFoundInClient: true
    }
  }

  apply_section $token "malware_blocker" {
    enabled: true
    sonarr: {enabled: true, blocklistType: "Blacklist", blocklistPath: $config.blocklistUrl}
    radarr: {enabled: true, blocklistType: "Blacklist", blocklistPath: $config.blocklistUrl}
  }

  ensure_stall_rule $token
  ensure_notification $token
  print "Cleanuparr reconcile complete"
}
