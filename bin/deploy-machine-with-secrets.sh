#!/usr/bin/env sh
# shellcheck disable=SC1091,SC2181
set -uf
SCRIPT_PATH="$(dirname "$0")"
# shellcheck source=util.sh
. "$SCRIPT_PATH"/util.sh

usage() {
  echo "deploy-machine-with-secrets.sh <flake-target> <host> <home-directory>

Example:
  nix-shell --packages bitwarden-cli jq --command ./bin/deploy-machine-with-secrets.sh .#laptop nixos@192.168.68.62 /home/bphenriques
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
if ! "$SCRIPT_PATH"/init-secrets.sh "$temp/$HOME_DIRECTORY"; then
  fail "Failed to initialize secrets. Aborting"
fi
nix run github:nix-community/nixos-anywhere -- --extra-files "$temp" --flake "${TARGET}" "${HOST}"
