#shellcheck shell=bash

set -e

# shellcheck disable=SC2016
usage() {
  echo 'Log in to and unlock Bitwarden CLI.'
  echo 'sh: export BW_SESSION="$(bw-session session <EMAIL>)"'
  echo 'fish: set -x BW_SESSION (bw-session session <EMAIL>)'
}

# Adapted from https://gist.github.com/seanh/d3d1a6dfa4b7d5d9f135984ae913cf0f
create_session() {
  test -z "$1" && echo "EMAIL not provided" && exit 1

  if ! bw login --check > /dev/null; then
    BW_SESSION="$(bw login --raw "$1")"
  fi

  if ! bw unlock --check > /dev/null; then
    BW_SESSION="$(bw unlock --raw)"
  fi

  bw sync > /dev/null
  echo "$BW_SESSION"
}

case "${1:-}" in
  ""|--help)      usage ;;
  session)        shift 1 && create_session "$@"  ;;
esac

