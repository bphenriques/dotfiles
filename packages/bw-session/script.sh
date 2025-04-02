#shellcheck shell=bash

set -e

# shellcheck disable=SC2016
usage() {
  echo 'Log in to and unlock Bitwarden CLI.'
  echo 'sh: export BW_SESSION="$(bw-session session <EMAIL>)"'
  echo 'fish: set -x BW_SESSION (bw-session session <EMAIL>)'
}


# FIXME
#create_sops_secret() {
#  bw get template item \
#    | jq --arg NAME "sops-age-key-system-$1-system" --arg FIELD "$(bw get template item.field | jq '.name')" '.name=$NAME' \
#    | bw encode \
#    | bw create item
#  bw get template item | jq --arg NAME="" ".name=env(SECRET_NAME))" | bw encode | bw create item
#  bw get template item | yq '.name = env(SECRET_NAME) | .fields = '
#}

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
  session)        shift 1 && create_session "$@"            ;;
  get-item)       shift 1 && bw get item "$1"  ;;
  get-item-field) shift 1 && bw get item "$1" | jq --arg FIELD "$2" --raw-output '.fields[] | select(.name == $FIELD) | .value'
esac

