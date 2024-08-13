#! /usr/bin/env nix-shell
#! nix-shell -i bash -p bitwarden-cli -p yq-go
#! nix-shell -I nixpkgs=https://github.com/NixOS/nixpkgs/archive/6a8a86ede2e8ce496648f2a8ae79d2c24464ca2a.tar.gz
# shellcheck shell=sh disable=SC2046,SC3028
SCRIPT_PATH="$(dirname "$0")"
set -e

DOTFILES_LOCATION="${SCRIPT_PATH}/.."


usage() {
  echo "nixos-remote-install.sh {host} {ssh_host} [--sops-age-file file]

  --sops-age-file            target location of the age files containing the private keys.
"
}

info() { printf '[ \033[00;34m..\033[0m ] %s\n' "$1"; }
fatal() { printf '[\033[0;31mFAIL\033[0m] %s\n' "$1" 1>&2; exit 1; }

host_require_secrets() {
  yq '.keys[] | anchor' < "${DOTFILES_LOCATION}"/.sops.yaml | grep -E "^$1$" > /dev/null 2>&1;
}

if [ $# -lt 3 ] || [ "$1" == "--help" ]; then
  usage
  exit 1
fi

host="$1"
ssh_host="$2"

sops_age_file=
while [ $# -gt 0 ]; do
  case "$1" in
    --sops-age-file)     sops_age_file="$2";      shift 2 ;;
    *) break ;;
  esac
done
! test -d "${DOTFILES_LOCATION}/hosts/${host}" && fatal "No matching '${host}' under '${DOTFILES_LOCATION}/hosts'"

EXTRA_FILES="$(mktemp -d)"
if host_require_secrets "$host"; then
  test -z "${sops_age_file}" && error "sops-age-file argument is required as '${host}' requires secrets!" && usage && exit 1

  info "'${host}' contains secrets. Setting private keys from Bitwarden"
  bw unlock --check > /dev/null || fatal "Vault must be unlocked"
  mkdir -p "$(dirname "${EXTRA_FILES}/${sops_age_file}")"
  bw get item "sops-age-key-${host}" | jq --raw-output '.fields[] | select(.name=="private") | .value' > "${EXTRA_FILES}/${sops_age_file}"
fi

nix run github:nix-community/nixos-anywhere -- --extra-files "$EXTRA_FILES" --flake ".#${host}" "${ssh_host}"
