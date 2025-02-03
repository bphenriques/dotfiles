# Secrets
# FIXME

SOPS_AGE_KEY_FILE="${SOPS_AGE_KEY_FILE:-"$HOME/.config/sops/age/keys.txt"}"

press_to_continue() { info 'Press any key to continue'; read -r _; }
usage() {
  echo "secrets.sh <host> [new]"
}

if [ "$1" = "--help" ]; then
  usage
  exit 1
fi

host="$1"

create_item() {
  bw get template item \
    | jq --arg NAME "sops-age-key-system-$1-system" --arg FIELD "$(bw get template item.field | jq '.name')" '.name=$NAME' \
    | bw encode \
    | bw create item


  bw get template item | jq --arg NAME="" ".name=env(SECRET_NAME))" | bw encode | bw create item

  bw get template item | yq '.name = env(SECRET_NAME) | .fields = '
}

case "${1:-}"
  new)
    target="$(mktemp)"
    age-keygen > "$target"
    cat ""
    cat "$target" >> "$SOPS_AGE_KEY_FILE"

    echo "Upload the "
    press_to_continue
    bw createa item <encodedJson> [options]

    bw get item "sops-age-key-system-${host}-system"

    rm "${target}"
    ;;
  ) ;;
easc


Adding new hosts requires:
1. Generate key pair using: `nix-shell -p age --command "age-keygen"`.
2. Export the private key to `$HOME/.config/sops/age/keys.txt` and upload to Bitwarden using the format `sops-age-key-$HOST-$USER` with a `private` field inside.
3. Add new host to `.sops.yaml` using the public key to `.sops.yaml` and the correct `path_regex`.
