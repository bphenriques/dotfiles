#!/usr/bin/env nu

let couchdb_url = $env.COUCHDB_URL
let admin_user = $env.COUCHDB_ADMIN_USER
let admin_pass = open $env.COUCHDB_ADMIN_PASS_FILE | str trim
let auth = $"($admin_user):($admin_pass)"

def wait_ready [] {
  for _ in 1..30 {
    try { http get $couchdb_url --max-time 2 | ignore; return } catch { sleep 1sec }
  }
  error make { msg: "CouchDB failed to start" }
}

def ensure_user [name: string, password_file: string] {
  let url = $"($couchdb_url)/_users/org.couchdb.user:($name)"
  let pw = open $password_file | str trim
  let body = { name: $name, password: $pw, roles: [], type: "user" }

  let r = http put $url $body --content-type application/json --user $auth --full --allow-errors
  if $r.status == 201 { return }
  if $r.status == 409 {
    let rev = (http get $url --user $auth)._rev
    http put $url ($body | insert _rev $rev) --content-type application/json --user $auth | ignore
    return
  }
  error make { msg: $"User ($name) failed: ($r.status)" }
}

def ensure_database [name: string, owner: string] {
  let url = $"($couchdb_url)/($name)"
  let r = http put $url --user $auth --full --allow-errors
  if $r.status not-in [201, 412] { error make { msg: $"Database ($name) failed: ($r.status)" } }

  let security = { admins: { names: [$owner], roles: [] }, members: { names: [$owner], roles: [] } }
  http put $"($url)/_security" $security --content-type application/json --user $auth | ignore
}

def main [] {
  wait_ready
  $env.COUCHDB_USERS_JSON | from json | each { |u| ensure_user $u.name $u.passwordFile } | ignore
  $env.COUCHDB_DBS_JSON | from json | each { |db| ensure_database $db.name $db.owner } | ignore
}
