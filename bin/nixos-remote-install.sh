#! /usr/bin/env nix-shell
#! nix-shell -i bash -p bitwarden-cli -p yq-go
#! nix-shell -I nixpkgs=https://github.com/NixOS/nixpkgs/archive/6a8a86ede2e8ce496648f2a8ae79d2c24464ca2a.tar.gz
# shellcheck shell=sh disable=SC2046,SC3028
SCRIPT_PATH="$(dirname "$0")"
set -e

DOTFILES_LOCATION="${SCRIPT_PATH}/.."
IMPERMANENCE_DIR="${IMPERMANENCE_DIR:-/persist/data/system}"
SOPS_AGE_SYSTEM_FILE="${IMPERMANENCE_DIR}/var/lib/sops-nix/system-keys.txt"

usage() {
  echo "nixos-remote-install.sh {host} {ssh_host} {bitwarden-email}

By default IMPERMANENCE_DIR is set to /persist/data/system but you can set to empty string if not used.
"
}

info() { printf '[ \033[00;34m..\033[0m ] %s\n' "$1"; }
fatal() { printf '[\033[0;31mFAIL\033[0m] %s\n' "$1" 1>&2; exit 1; }

import_age_system_private_key() {
  host="$1"
  target="$2"
  if yq '.keys[] | anchor' < "${DOTFILES_LOCATION}"/.sops.yaml | grep -E "^${host}-system$" > /dev/null; then
    info "'${host}' contains system wide private keys. Getting private keys from Bitwarden"
    bw unlock --check > /dev/null || fatal "Vault must be unlocked"

    mkdir -p "$(dirname "${target}")"
    bw get item "sops-age-key-${host}-system" \
      | jq --raw-output '.fields[] | select(.name=="private") | .value' \
      > "${target}"
  fi
}

if [ "$1" == "--help" ]; then
  usage
  exit 1
fi

host="$1"
ssh_host="$2"
bw_email="$3"

BW_SESSION="$("${SCRIPT_PATH}"/../apps/bw-session.sh "${bw_email}")"
export BW_SESSION

! test -d "${DOTFILES_LOCATION}/hosts/${host}" && fatal "No matching '${host}' under '${DOTFILES_LOCATION}/hosts'"
EXTRA_FILES="$(mktemp -d)"
import_age_system_private_key "$host" "${EXTRA_FILES}/${SOPS_AGE_SYSTEM_FILE}"

nix run github:nix-community/nixos-anywhere -- --extra-files "$EXTRA_FILES" --flake ".#${host}" "${ssh_host}"
