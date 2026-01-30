#!/usr/bin/env nu

let couchdb_url = $env.COUCHDB_URL
let admin_user = $env.COUCHDB_ADMIN_USER
let admin_pass = open $env.COUCHDB_ADMIN_PASS_FILE | str trim

def wait_ready [] {
  for _ in 1..30 {
    try { http get $"($couchdb_url)/_up" --max-time 2sec | ignore; return } catch { sleep 1sec }
  }
  error make { msg: "CouchDB failed to start" }
}

def ensure_user [name: string, password_file: string] {
  let url = $"($couchdb_url)/_users/org.couchdb.user:($name)"
  let pw = open $password_file | str trim
  let body = { name: $name, password: $pw, roles: [], type: "user" }

  let r = http put $url $body --content-type application/json --user $admin_user --password $admin_pass --full --allow-errors
  if $r.status == 201 { return }
  if $r.status == 409 {
    let rev = (http get $url --user $admin_user --password $admin_pass)._rev
    let update = http put $url ($body | insert _rev $rev) --content-type application/json --user $admin_user --password $admin_pass --full --allow-errors
    if $update.status != 201 { error make { msg: $"User ($name) update failed: ($update.status)" } }
    return
  }
  error make { msg: $"User ($name) failed: ($r.status)" }
}

def ensure_database [name: string, owner: string] {
  let url = $"($couchdb_url)/($name)"
  let r = http put $url --user $admin_user --password $admin_pass --content-type "application/json" --full --allow-errors {}
  if $r.status not-in [201, 412] { error make { msg: $"Database ($name) failed: ($r.status)" } }

  let security = { admins: { names: [$owner], roles: [] }, members: { names: [$owner], roles: [] } }
  let sr = http put $"($url)/_security" $security --content-type application/json --user $admin_user --password $admin_pass --full --allow-errors
  if $sr.status != 200 { error make { msg: $"Database ($name) security config failed: ($sr.status)" } }
}

def ensure_single_node_setup [] {
  let url = $"($couchdb_url)/_cluster_setup"
  let r = http get $url --user $admin_user --password $admin_pass --full --allow-errors
  if $r.status != 200 { error make { msg: $"Cluster status check failed: ($r.status)" } }

  let status = $r.body.state
  if $status == "cluster_finished" or $status == "single_node_enabled" { return }

  let bind_address = $env.COUCHDB_BIND_ADDRESS
  let port = $env.COUCHDB_PORT | into int
  let body = { action: "enable_single_node", bind_address: $bind_address, port: $port, singlenode: true }
  let setup = http post $url $body --content-type application/json --user $admin_user --password $admin_pass --full --allow-errors
  if $setup.status != 201 { error make { msg: $"Cluster setup failed: ($setup.status) ($setup.body)" } }
}

def main [] {
  wait_ready
  ensure_single_node_setup
  $env.COUCHDB_USERS_JSON | from json | each { |u| ensure_user $u.name $u.passwordFile } | ignore
  $env.COUCHDB_DBS_JSON | from json | each { |db| ensure_database $db.name $db.owner } | ignore
}
