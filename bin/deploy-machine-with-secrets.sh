#!/usr/bin/env sh
# shellcheck disable=SC1091,SC2181
set -uf
SCRIPT_PATH="$(dirname "$0")"
# shellcheck source=util.sh
. "$SCRIPT_PATH"/util.sh

usage() {
  echo "deploy-machine-with-secrets.sh <flake-target> <host> <home-directory>

E.g.:
- Remote machine: deploy-machine-with-secrets.sh .#laptop nixos@192.168.68.62 /home/bphenriques
"
}

if [ "$#" -ne 3 ] || [ "$1" == "--help" ]; then
  usage
  exit
fi

TARGET="$1"
HOST="$2"
HOME_DIRECTORY="$3"

temp="$(mktemp -d)"
"$SCRIPT_PATH"/init-secrets.sh "$temp/$HOME_DIRECTORY"
nix run github:nix-community/nixos-anywhere -- --extra-files "$temp" --flake "${TARGET}" "${HOST}"
