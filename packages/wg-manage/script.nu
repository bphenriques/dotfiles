#!/usr/bin/env nu

# WireGuard client management (IPv4 only)
#
# Required env: WG_DATA_DIR, WG_INTERFACE, WG_SERVER_ENDPOINT, WG_SERVER_IP, WG_CLIENT_SUBNET, WG_CLIENT_DNS
# Optional env: WG_SERVER_ALLOWED_IPS (defaults to WG_CLIENT_SUBNET)
# Email env: WG_SMTP_URL_FILE, WG_SMTP_FROM, WG_EMAIL_TEMPLATE_FILE, WG_EMAIL_SUBJECT

def require_env [name: string] {
  let val = ($env | get -o $name)
  if ($val == null or ($val | is-empty)) {
    error make { msg: $"($name) required" }
  } else {
    $val
  }
}

let data_dir = require_env "WG_DATA_DIR"
let interface = require_env "WG_INTERFACE"
let endpoint = require_env "WG_SERVER_ENDPOINT"
let server_ip = require_env "WG_SERVER_IP"
let client_subnet = require_env "WG_CLIENT_SUBNET"
let client_dns = require_env "WG_CLIENT_DNS"
let allowed_ips_full = $env.WG_SERVER_ALLOWED_IPS? | default $client_subnet
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
  # Assumes IPv4 /24 subnet
  let parts = ($client_subnet | split row "/")
  let base_ip = ($parts | get 0)
  let cidr = ($parts | get 1 | into int)
  if $cidr != 24 {
    error make { msg: $"Only /24 subnets supported (got /($cidr))" }
  }

  let prefix = ($base_ip | split row "." | slice 0..2 | str join ".")
  let used = (list_clients | get -o ip | default [] | each {|ip| $ip | split row "." | get 3 | into int })
  let next = (2..254 | where {|n| not ($n in $used) } | get 0?)

  if $next == null {
    error make { msg: "No free IPs" }
  } else {
    $"($prefix).($next)"
  }
}

def render_conf [priv_key: string, ip: string] {
  $"[Interface]
PrivateKey = ($priv_key)
Address = ($ip)/32
DNS = ($client_dns)

[Peer]
PublicKey = (get_server_pubkey)
Endpoint = ($endpoint)
AllowedIPs = ($allowed_ips_full)
PersistentKeepalive = 25
"
}

def send_email [name: string, to: string] {
  let smtp_url_file = require_env "WG_SMTP_URL_FILE"
  let smtp_url = (open --raw $smtp_url_file | str trim)
  let from = require_env "WG_SMTP_FROM"
  let template_file = require_env "WG_EMAIL_TEMPLATE_FILE"
  let subject = require_env "WG_EMAIL_SUBJECT"
  let dir = $"($clients_dir)/($name)"
  let tmpdir = (mktemp --tmpdir -d | str trim)
  let qr = $"($tmpdir)/wireguard.png"
  open --raw $"($dir)/client.conf" | qrencode -s 6 -o $qr # double the size to make the QR code bigger

  # Template is pre-rendered HTML at build time; just substitute the name
  let body = (open --raw $template_file | str replace --all "{{NAME}}" $name)

  let mutt_cfg = $"set from=($from); set smtp_url=($smtp_url); set ssl_starttls=yes; set content_type=text/html"
  echo $body | mutt -s $subject -e $mutt_cfg -a $"($dir)/client.conf" -a $qr -- $to
  let exit_code = $env.LAST_EXIT_CODE

  rm -r $tmpdir

  if $exit_code != 0 {
    print $"Error sending email to ($to) \(exit code: ($exit_code)\)"
  } else {
    print $"Email sent to ($to)"
  }
}

# Not thread-safe: concurrent calls may assign the same IP
def create_client [name: string, --email: string, --ip: string] {
  let dir = $"($clients_dir)/($name)"
  let client_ip = if $ip == null { next_ip } else { $ip }
  mkdir $dir

  let priv_key = (wg genkey | str trim)
  let pub_key = ($priv_key | wg pubkey | str trim)
  $priv_key | save -f $"($dir)/private.key"; chmod 0600 $"($dir)/private.key"

  mut meta = { name: $name, ip: $client_ip, pub_key: $pub_key }
  if $email != null { $meta = ($meta | insert email $email) }
  $meta | save -f $"($dir)/meta.json"

  render_conf $priv_key $client_ip | save -f $"($dir)/client.conf"; chmod 0600 $"($dir)/client.conf"
  wg set $interface peer $pub_key allowed-ips $"($client_ip)/32"

  print $"Client '($name)' added (($client_ip))"
}

def get_client [name: string] {
  let dir = $"($clients_dir)/($name)"
  if not ($dir | path exists) {
    error make { msg: $"Client '($name)' not found" }
  }
  open $"($dir)/meta.json"
}

def "main list" [] {
  let clients = list_clients
  if ($clients | is-empty) {
    print "No clients"
  } else {
    $clients | select name ip | print
  }
}

def "main show" [name: string] {
  get_client $name | ignore  # validates client exists
  show_qr $name
}

def "main email" [name: string] {
  let client = get_client $name
  let email = $client.email? | if ($in == null or ($in | is-empty)) {
    error make { msg: $"Client '($name)' has no email" }
  } else { $in }
  send_email $name $email
}

def "main add" [name: string, email?: string] {
  let dir = $"($clients_dir)/($name)"
  if ($dir | path exists) {
    print $"Client '($name)' exists"
  } else {
    if $email != null {
      create_client $name --email $email
    } else {
      create_client $name
    }
  }

  if $email != null {
    send_email $name $email
  } else {
    show_qr $name
  }
}

def "main remove" [...names: string] {
  for name in $names {
    let dir = $"($clients_dir)/($name)"
    if not ($dir | path exists) {
      print $"Client '($name)' not found"
      continue
    }
    let meta = open $"($dir)/meta.json"
    rm -r $dir
    wg set $interface peer $meta.pub_key remove
    print $"Client '($name)' removed"
  }
}

def "main bootstrap" [config_file: path] {
  # Create clients from config (skips existing)
  for entry in (open $config_file) {
    let dir = $"($clients_dir)/($entry.name)"
    if ($dir | path exists) {
      print $"Skipping '($entry.name)'"
      continue
    }

    let email = $entry.email? | if ($in == null or ($in | is-empty)) { null } else { $in }
    create_client $entry.name --ip $entry.ip --email $email
    if $email != null { send_email $entry.name $email }
  }

  # Sync peers to WireGuard interface
  let live_peers = get_live_peers
  let clients = list_clients
  let file_pubkeys = ($clients | get -o pub_key | default [])

  for c in ($clients | where { $in.pub_key not-in $live_peers }) {
    wg set $interface peer $c.pub_key allowed-ips $"($c.ip)/32"
    print $"Synced: ($c.name)"
  }

  for p in ($live_peers | where { $in not-in $file_pubkeys }) {
    print $"Warning: unknown peer ($p | str substring 0..8)..."
  }
}

def main [] {
  print "wg-manage - WireGuard client management

  list                   List clients
  add <name> [email]     Add client (restricted access, email optional)
  show <name>            Show QR code for client
  email <name>           Send config email (client must have email)
  remove <name>...       Remove one or more clients
  bootstrap <file>       Bootstrap clients from JSON and sync peers"
}
