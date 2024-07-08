#!/usr/bin/env sh
# shellcheck disable=SC2016,SC1091,SC1090,SC2002
#
# Remotely deploys a flake
#
set -euf

# Constants
BITWARDEN_SHARED_KEY_ID="nix-sops age shared key"
SSH_KEY_EMAIL_ADDRESS="4727729+bphenriques@users.noreply.github.com"
SSH_TYPE=ed25519

# Utility methods
info() {
  # shellcheck disable=SC2059
  printf '\r[ \033[00;34m..\033[0m ] %s\n' "$1"
}

success() {
  # shellcheck disable=SC2059
  printf '\r\033[2K  [ \033[00;32mOK\033[0m ] %s\n' "$1"
}

fail() {
  # shellcheck disable=SC2059
  printf '\r\033[2K[\033[0;31mFAIL\033[0m] %s\n' "$1" 1>&2 # Redirect to stderror
  exit 1
}

usage() {
  echo "init-secrets.sh <home-directory>

E.g.:
- Existing machine: ./init-secrets.sh /home/bphenriques
- To provision with nixos-anywhere: ./init-secrets.sh /tmp/random-directory/home/bphenriques
  "
}

check_bw_login() {
  if ! bw login --check >/dev/null; then
    echo "You are not logged in: bw login"
    return 1
  fi
  if ! bw unlock --check >/dev/null; then
    echo "The vault is locked: bw unlock"
    return 1
  fi
}

initialize_github_ssh_key() {
  local target="$1"
  if [ -f "${target}" ]; then
    success 'Github SSH Key - Already installed!'
  else
    info 'Github SSH Key - Initializing a new one for the host!'
    mkdir -p "$(dirname "$target")"
    ssh-keygen -t "$SSH_TYPE" -C "$SSH_KEY_EMAIL_ADDRESS" -f "$target"
    success 'Github SSH Key - Installed!'
  fi
}

upload_ssh_key_github() {
  local title="$1"
  local target="$2"
  curl -L \
    -X POST \
    -H "Accept: application/vnd.github+json" \
    -H "Authorization: Bearer ${GITHUB_TOKEN}" \
    -H "X-GitHub-Api-Version: 2022-11-28" \
    https://api.github.com/user/keys \
    -d "{\"title\":\"title\", \"key\":\"$(cat "${target}.pub")\"}"
}

initialize_age_key() {
  local target="$1"
  if [ -f "${target}" ]; then
    success 'Shared Age Key - Already present!'
  else
    info 'Shared Age Key - Adding shared key by getting it from Bitwarden!'
    check_bw_login
    mkdir -p "$(dirname "$target")"
    bw get item "$BITWARDEN_SHARED_KEY_ID" | jq --raw-output '.fields[] | select(.name=="private-key") | .value' >> "${target}"
    success 'SSH Key - Installed!'
  fi
}

command -v bw > /dev/null || fail "bitwarden-cli is not available"
command -v jq > /dev/null || fail "jq is not available"

if [ "$#" -ne 1 ] || [ "$1" == "--help" ]; then
  usage
  exit 1
fi

HOME_LOCATION="$1"

info "Bootstrapping secrets under $HOME_LOCATION"
temp="$(mktemp -d)"
initialize_github_ssh_key "$HOME_LOCATION/.ssh/id_$SSH_TYPE"
initialize_age_key "$HOME_LOCATION/.config/sops/age/keys.txt"
success "Complete!"
