#! /usr/bin/env nix-shell
#! nix-shell -i bash -p bitwarden-cli -p yq-go
#! nix-shell -I nixpkgs=https://github.com/NixOS/nixpkgs/archive/6a8a86ede2e8ce496648f2a8ae79d2c24464ca2a.tar.gz
# shellcheck shell=sh disable=SC2046,SC3028
SCRIPT_PATH="$(dirname "$0")"
set -e

DOTFILES_LOCATION="${SCRIPT_PATH}/.."
IMPERMANENCE_DIR="/persist/data/system"
SOPS_AGE_VAR_LIB_DIR="${IMPERMANENCE_DIR}/var/lib/sops-nix"

usage() {
  echo "nixos-remote-install.sh {host} {ssh_host}"
}

info() { printf '[ \033[00;34m..\033[0m ] %s\n' "$1"; }
fatal() { printf '[\033[0;31mFAIL\033[0m] %s\n' "$1" 1>&2; exit 1; }

add_host_secrets() {
  host="$1"
  directory="$2"
  if yq '.keys[] | anchor' < "${DOTFILES_LOCATION}"/.sops.yaml | grep -E "^$1" > /dev/null; then
    info "'${host}' contains secrets. Getting private keys from Bitwarden"
    bw unlock --check > /dev/null || fatal "Vault must be unlocked"

    mkdir -p "${directory}"
    for secret_type in $(yq '.keys[] | anchor' < "${DOTFILES_LOCATION}"/.sops.yaml | grep -E "^${host}" | sed "s/${host}-//g"); do
      info "Fetching secrets of type '${secret_type}'"
      bw get item "sops-age-key-${host}-${secret_type}" \
        | jq --raw-output '.fields[] | select(.name=="private") | .value' \
        > "${directory}/${secret_type}-keys.txt"
    done
  fi
}

if [ "$1" == "--help" ]; then
  usage
  exit 1
fi

host="$1"
ssh_host="$2"
shift 2

! test -d "${DOTFILES_LOCATION}/hosts/${host}" && fatal "No matching '${host}' under '${DOTFILES_LOCATION}/hosts'"
EXTRA_FILES="$(mktemp -d)"
add_host_secrets "$host" "${EXTRA_FILES}/${SOPS_AGE_VAR_LIB_DIR}"

nix run github:nix-community/nixos-anywhere -- --extra-files "$EXTRA_FILES" --flake ".#${host}" "${ssh_host}"
