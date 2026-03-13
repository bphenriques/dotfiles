#shellcheck shell=bash
set -euo pipefail

fatal()   { printf '[FAIL] %s\n' "$1" >&2; exit 1; }
info()    { printf '[ .. ] %s\n' "$1"; }
success() { printf '[ OK ] %s\n' "$1"; }

bw_get_item_field() { bw get item "$1" | jq -e --arg FIELD "$2" --raw-output '.fields[] | select(.name == $FIELD) | .value'; }

fetch() {
  local secret_type="${1:-}"
  local host="${2:-}"
  
  test -z "$secret_type" && fatal "secret-type argument not provided"

  # GPG keys are shared (not per-host)
  case "$secret_type" in
    gpg-private-key)  bw get item "github-gpg-private" | jq -re '.notes' ; return ;;
    gpg-public-key)   bw get item "github-gpg-public" | jq -re '.notes' ; return ;;
  esac

  # All other secrets require a host
  test -z "$host" && fatal "host argument not provided"

  case "$secret_type" in
    sops-secret)      bw_get_item_field "system-nixos-${host}" "sops-private" ;;
    luks-key)         bw_get_item_field "system-nixos-${host}" "luks-interactive-password" ;;
    ssh-private-key)  bw get item "ssh-key-nixos-${host}" | jq -re '.sshKey.privateKey' ;;
    sops-private-key) bw_get_item_field "system-nixos-${host}" "sops-private" ;;
    *)                fatal "Unknown secret type: $secret_type" ;;
  esac
}

init_host() {
  local host="${1:-}"
  local with_luks="${2:-}"
  test -z "$host" && fatal "host argument not provided"
  
  local item_name
  item_name="system-nixos-$host"
  
  # Check if item already exists
  if bw get item "$item_name" > /dev/null 2>&1; then
    fatal "Bitwarden item '$item_name' already exists. Delete it first or use a different host name."
  fi
  
  info "Generating SOPS age key..."
  local tmpdir
  tmpdir="$(mktemp -d)"
  trap 'rm -rf "$tmpdir"' EXIT

  # Build fields array. Optionally add LUKS key
  age-keygen -o "$tmpdir/age.key" 2>/dev/null
  local fields
  fields=$(jq -n --arg sops "$(cat "$tmpdir/age.key")" '[{name: "sops-private", value: $sops, type: 0}]')

  local luks_password=""
  if [ "$with_luks" = "--luks" ]; then
    info "Generating LUKS password..."
    luks_password="$(openssl rand -base64 32)"
    fields=$(echo "$fields" | jq --arg luks "$luks_password" '. += [{name: "luks-interactive-password", value: $luks, type: 1}]')
  fi
  
  info "Creating Bitwarden item '$item_name'..."
  local item_json
  item_json=$(jq -n \
    --arg name "$item_name" \
    --argjson fields "$fields" \
    '{type: 2, secureNote: {type: 0}, name: $name, fields: $fields}')
  
  echo "$item_json" | bw encode | bw create item > /dev/null
  bw sync > /dev/null
  
  success "Created Bitwarden item: $item_name"
  echo ""
  echo "SOPS public key: $(age-keygen -y "$tmpdir/age.key")"
  if [ -n "$luks_password" ]; then
    echo "LUKS password: Check field 'luks-interactive-password' in Bitwarden"
  fi
  
  echo "Next steps:"
  echo "  1. Create hosts/${host}/.sops.yaml with the public key above"
  echo "  2. Create hosts/${host}/secrets.yaml and encrypt with: sops hosts/${host}/secrets.yaml"
  echo "  3. Apply the changes"
  rm -rf "$tmpdir"
}

usage() {
  cat <<'EOF'
Manage Bitwarden secrets for NixOS provisioning.

Usage: dotfiles-secrets <command> [args]

Commands:
  init-host <host> [--luks]    Set up secrets for a new host
  fetch <type> [host]          Retrieve secret value
  exists <type> [host]         Check if secret exists (exit 0/1)

Secret Types:
  sops-secret       Age private key for sops-nix (per-host)
  luks-key          LUKS disk encryption password (per-host)
  gpg-private-key   GPG private key (shared)
  gpg-public-key    GPG public key (shared)

Examples:
  dotfiles-secrets init-host laptop              # New host without LUKS
  dotfiles-secrets init-host homelab/compute --luks
  dotfiles-secrets fetch sops-secret laptop
  dotfiles-secrets fetch gpg-private-key

Bitwarden Structure:
  Host "homelab/compute" creates item "system-nixos-homelab-compute" with fields:
    - sops-private: Age private key
    - luks-interactive-password: LUKS password (if --luks)
EOF
  exit 1
}

unlock_bitwarden() {
  local bw_email="$1"
  info "Unlocking Bitwarden account (${bw_email})..."
  BW_SESSION="$(bw-session session "$bw_email")"
  export BW_SESSION
}

unlock_bitwarden "$1"
shift
case "${1:-}" in
  -h|--help|"") usage ;;
  fetch)        shift && fetch "$@" ;;
  exists)       shift && fetch "$@" > /dev/null 2>&1 ;;
  init-host)    shift && init_host "$@" ;;
  *)            fatal "Unknown command: $1" ;;
esac
