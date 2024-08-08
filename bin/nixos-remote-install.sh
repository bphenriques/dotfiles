#! /usr/bin/env nix-shell
#! nix-shell -i bash -p bitwarden-cli -p yq-go
#! nix-shell -I nixpkgs=https://github.com/NixOS/nixpkgs/archive/6a8a86ede2e8ce496648f2a8ae79d2c24464ca2a.tar.gz
# shellcheck shell=sh disable=SC2046,SC3028
SCRIPT_PATH="$(dirname "$0")"

# ./bin/nixos-remote-install.sh laptop ip --sops-age-destination /persist/data/home/bphenriques/.config/sops/age/keys.txt

set -e

usage() {
  echo "Setups a nixos machine

Usage: nixos-installer.sh [build|install]

  --host            matching directory name under  match on of the directories under $DOTFILES_LOCATION/hosts/

Options:
"
}

info() { printf '[ \033[00;34m..\033[0m ] %s\n' "$1"; }
success() { printf '[ \033[00;32mOK\033[0m ] %s\n' "$1"; }
warn() { printf '[ \033[01;33mWARN\033[0m ] %s\n' "$1"; }
error() { printf '[\033[0;31mERROR\033[0m] %s\n' "$1" 1>&2; }
fatal() { printf '[\033[0;31mFAIL\033[0m] %s\n' "$1" 1>&2; exit 1; }
press_to_continue() { info 'Press any key to continue'; read -r _; }

host_require_secrets() {
  host="$1"
  dotfiles_location="$2"

  yq '.keys[] | anchor' < "${dotfiles_location}"/.sops.yaml | grep -E "^${host}$" > /dev/null 2>&1;
}

fetch_age_private_key() {
  bw get item "sops-age-key-$1" | jq --raw-output '.fields[] | select(.name=="private") | .value'
}

dotfiles_location="$HOME"/.dotfiles
host="$1"
ssh_host="$2"
shift 2

sops_age_destination=
while [ $# -gt 0 ]; do
  case "$1" in
    --help)                     usage;                          exit  0 ;;
    --sops-age-destination)     sops_age_destination="$2";      shift 2 ;;
    *) break ;;
  esac
done
test -z "${host}" && error "host is not set!" && usage && exit 1
! test -d "${dotfiles_location}/hosts/${host}" && fatal "No matching '${host}' under '${dotfiles_location}/hosts'"
test -z "${ssh_host}" && error "ssh_host is not set!" && usage && exit 1
test -z "${BW_SESSION}" && error "BW_SESSION is not set. Unlock your bitwarden vault to continue." && usage && exit 1

file_tree_to_copy="$(mktemp -d)"
if host_require_secrets "$host" "${dotfiles_location}"; then
  test -z "${sops_age_destination}" && error "sops-age-destination argument is required as '${host}' requires secrets!" && usage && exit 1

  info "${host} requires secret. Fetching from Bitwarden"
  mkdir -p "$(dirname "${file_tree_to_copy}/${sops_age_destination}")"
  fetch_age_private_key "${host}" > "${file_tree_to_copy}/${sops_age_destination}"
  # TODO: should I run this only after the file is copied?
  #chmod 600 "${file_tree_to_copy}/${sops_age_destination}"
else
  info "${host} does not require secrets."
fi

tree -a ${file_tree_to_copy}
#nix run github:nix-community/nixos-anywhere -- --extra-files "$file_tree_to_copy" --flake "#.${host}" "${ssh_host}"
