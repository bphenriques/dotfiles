#shellcheck shell=bash

fatal() { printf '[FAIL] %s\n' "$1" 1>&2; exit 1; }
bw_get_item_field() { bw get item "$1" | jq -e --arg FIELD "$2" --raw-output '.fields[] | select(.name == $FIELD) | .value'; }

# Note: Bitwarden doesn't support exact match. I don't want to use `ids`, therefore I adjusted the secret names.
fetch() {
  test -z "$2" && fatal "host argument not provided."
  case "$1" in
    luks-key)         bw_get_item_field "system-nixos-$2" "luks-interactive-password"       ;;
    sops-secret)      bw_get_item_field "system-nixos-$2" "sops-private"                    ;;
    ssh-private-key)  bw get item "ssh-key-nixos-$2" | jq -re '.sshKey.privateKey'          ;;
    gpg-private-key)  bw get item "github-gpg-private" | jq -re '.notes'                    ;;
    gpg-public-key)   bw get item "github-gpg-public" | jq -re '.notes'                     ;;
  esac
}

if ! bw unlock --check > /dev/null; then
  echo "Bitwarden not unlocked"
  exit 1
fi
bw sync > /dev/null

case "$1" in
  fetch)  shift 1 && fetch "$@" ;;
  exists) shift 1 && fetch "$@" > /dev/null ;;
esac
