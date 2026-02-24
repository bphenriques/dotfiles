#!/usr/bin/env nu

# WireGuard client management (IPv4 only)
#
# Required env: WG_DATA_DIR, WG_INTERFACE, WG_SERVER_ENDPOINT, WG_CLIENT_SUBNET, WG_CLIENT_DNS
# Optional env: WG_SERVER_ALLOWED_IPS (defaults to WG_CLIENT_SUBNET), WG_SMTP_FROM (for email)

def require_env [name: string] {
  $env | get -o $name | if ($in == null or ($in | is-empty)) { error make { msg: $"($name) required" } } else { $in }
}

let data_dir = require_env "WG_DATA_DIR"
let interface = require_env "WG_INTERFACE"
let endpoint = require_env "WG_SERVER_ENDPOINT"
let client_subnet = require_env "WG_CLIENT_SUBNET"
let client_dns = require_env "WG_CLIENT_DNS"
let allowed_ips = $env.WG_SERVER_ALLOWED_IPS? | default $client_subnet
let clients_dir = $"($data_dir)/clients"
let server_pubkey_file = $"($data_dir)/server/public.key"

if not ($server_pubkey_file | path exists) { error make { msg: "Server key not found. Start wireguard service first." } }

def get_server_pubkey [] { open --raw $server_pubkey_file | str trim }
def get_live_peers [] { wg show $interface dump | lines | skip 1 | each { $in | split row "\t" | get 0 } }
def show_qr [name: string] { open --raw $"($clients_dir)/($name)/client.conf" | qrencode -t ANSIUTF8 }

def list_clients [] {
  if not ($clients_dir | path exists) { return [] }
  ls $clients_dir
    | where type == dir
    | each { |d| $"($d.name)/meta.json" }
    | where { $in | path exists }
    | each { open $in }
}

def next_ip [] {
  let prefix = $client_subnet | split row "/" | get 0 | split row "." | slice 0..2 | str join "."
  let used = list_clients | get -o ip | default [] | each { $in | split row "." | get 3? | default "0" | into int }
  let next = 2..254 | where { |n| not ($n in $used) } | get 0? | if ($in == null) { error make { msg: "No free IPs" } } else { $in }
  $"($prefix).($next)"
}

def render_conf [priv_key: string, ip: string] {
  $"[Interface]
PrivateKey = ($priv_key)
Address = ($ip)/32
DNS = ($client_dns)

[Peer]
PublicKey = (get_server_pubkey)
Endpoint = ($endpoint)
AllowedIPs = ($allowed_ips)
PersistentKeepalive = 25
"
}

def send_email [name: string, to: string] {
  let from = require_env "WG_SMTP_FROM"
  let dir = $"($clients_dir)/($name)"
  let qr = mktemp --suffix=.png | str trim
  open --raw $"($dir)/client.conf" | qrencode -o $qr
  echo $"WireGuard config for '($name)' attached." | mutt -s $"WireGuard: ($name)" -e $"set from=($from)" -a $"($dir)/client.conf" -a $qr -- $to
  rm $qr
  print $"Email sent to ($to)"
}

# Not thread-safe: concurrent calls may assign the same IP
def create_client [name: string] {
  let dir = $"($clients_dir)/($name)"
  let ip = next_ip
  mkdir $dir

  let priv_key = wg genkey | str trim
  let pub_key = $priv_key | wg pubkey | str trim
  $priv_key | save -f $"($dir)/private.key"; chmod 0600 $"($dir)/private.key"
  { name: $name, ip: $ip, pub_key: $pub_key } | save -f $"($dir)/meta.json"

  render_conf $priv_key $ip | save -f $"($dir)/client.conf"; chmod 0600 $"($dir)/client.conf"
  wg set $interface peer $pub_key allowed-ips $"($ip)/32"

  print $"Client '($name)' added (($ip))"
}

def "main list" [] {
  print "NAME\tIP"
  list_clients | each { |c| print $"($c.name)\t($c.ip)" }
}

def "main add" [name: string, email?: string] {
  let dir = $"($clients_dir)/($name)"
  if ($dir | path exists) { print $"Client '($name)' exists" } else { create_client $name }
  if $email != null { send_email $name $email } else { show_qr $name }
}

def "main remove" [name: string] {
  let dir = $"($clients_dir)/($name)"
  if not ($dir | path exists) { error make { msg: $"Client '($name)' not found" } }
  let meta = open $"($dir)/meta.json"
  rm -r $dir
  wg set $interface peer $meta.pub_key remove
  print $"Client '($name)' removed"
}

def "main bootstrap" [config_file?: path] {
  if $config_file != null {
    for name in (open $config_file) {
      if ($"($clients_dir)/($name)" | path exists) { print $"Skipping '($name)'" } else { create_client $name }
    }
  }

  let live_peers = get_live_peers
  let clients = list_clients
  let file_pubkeys = $clients | get -o pub_key | default []

  for c in ($clients | where { $in.pub_key not-in $live_peers }) {
    wg set $interface peer $c.pub_key allowed-ips $"($c.ip)/32"
    print $"Added: ($c.name)"
  }

  for p in ($live_peers | where { $in not-in $file_pubkeys }) {
    print $"Warning: unknown peer ($p | str substring 0..8)..."
  }
}

def main [] {
  print "wg-manage - WireGuard client management

  list                 List clients
  add <name> [email]   Add client (show QR, or email if provided)
  remove <name>        Remove client
  bootstrap [file]     Bootstrap from JSON and sync peers"
}
