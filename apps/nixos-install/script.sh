#!/usr/bin/env sh

DOTFILES_LOCATION="${DOTFILES_LOCATION:-$HOME/.dotfiles}"
SOPS_AGE_SYSTEM_FILE="/var/lib/sops-nix/system-keys.txt"

usage() {
  echo "nixos-remote-install.sh {host} {bitwarden-email} [--remotely {ssh_host}] [--locally]"
}

info() { printf '[ \033[00;34m  \033[0m ] %s\n' "$1"; }
error() { printf '[\033[0;31mERROR\033[0m] %s\n' "$1" 1>&2; }
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

if [ "$1" = "--help" ]; then
  usage
  exit 1
fi

local_install() {
  info "TBD"
}

remote_install() {
  host="$1"
  ssh_host="$2"
  ! test -d "${DOTFILES_LOCATION}" && fatal "dotfiles folder not found: ${DOTFILES_LOCATION}"
  ! test -d "${DOTFILES_LOCATION}/hosts/${host}" && fatal "No matching '${host}' under '${DOTFILES_LOCATION}/hosts'"

  extra_files="$(mktemp -d)"
  import_age_system_private_key "$host" "${extra_files}/${SOPS_AGE_SYSTEM_FILE}"

  nix run github:nix-community/nixos-anywhere -- --extra-files "$extra_files" --flake ".#${host}" "${ssh_host}"
}

host="$1"
bw_email="$2"

shift 2

BW_SESSION="$(bw-session session "${bw_email}")"
export BW_SESSION
case "$1" in
  --remotely) remote_install "$host" "$1"                       ;;
  --locally)  local_install "$host"                             ;;
  *)          error "Invalid argument $1" && usage && exit 1    ;;
esac
