#shellcheck shell=bash

# Adapted from https://gist.github.com/seanh/d3d1a6dfa4b7d5d9f135984ae913cf0f
#
# Log in to and unlock Bitwarden CLI.
#
# Usage (sh): export BW_SESSION="$(bw-session <EMAIL>)"
# Usage (fish): set -x BW_SESSION (bw-session <EMAIL>)
set -e

BW_PASSWORD=${BW_PASSWORD:-}

test -z "$1" && echo "EMAIL not provided" && exit 1

if ! bw login --check > /dev/null; then
  BW_SESSION="$(bw login --raw "$1" "$BW_PASSWORD")"
fi

if ! bw unlock --check > /dev/null; then
  BW_SESSION="$(bw unlock --raw "$BW_PASSWORD")"
fi

bw sync > /dev/null
echo "$BW_SESSION"
