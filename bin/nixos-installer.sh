#!/usr/bin/env sh
# shellcheck disable=SC1091,SC2181
set -uf
SCRIPT_PATH="$(dirname "$0")"
# shellcheck source=util.sh
. "$SCRIPT_PATH"/util.sh

usage() {
  echo "deploy-machine-with-secrets.sh <flake-target> <host> <home-directory>

Example:

  1. Start by installing the dependencies: nix-shell --packages bitwarden-cli jq
  2. Then login using: bw login && bw unlock
  3. Then:
    1. From the NixOS installer: ./bin/nixos-installer.sh .#laptop nixos@192.168.68.62 /persist/config/bphenriques/home/bphenriques
    2. From an already running machine: ./bin/nixos-installer.sh .#laptop bphenriques@192.168.68.62 /persist/config/bphenriques/home/bphenriques
"
}

if [ "$#" -ne 3 ] || [ "$1" == "--help" ]; then
  usage
  exit 1
fi

TARGET="$1"
HOST="$2"
HOME_DIRECTORY="$3"

temp="$(mktemp -d)"
if ! "$SCRIPT_PATH"/init-keys.sh "$temp/$HOME_DIRECTORY"; then
  fail "Failed to initialize keys. Aborting"
fi
nix run github:nix-community/nixos-anywhere -- --extra-files "$temp" --flake "${TARGET}" "${HOST}"
