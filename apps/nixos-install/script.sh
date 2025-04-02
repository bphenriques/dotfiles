#!/usr/bin/env sh

BRANCH_NAME="${BRANCH_NAME:-main}"
FLAKE_URL="${FLAKE_URL:-github:bphenriques/dotfiles/${BRANCH_NAME}}"
DOTFILES_LOCATION="${DOTFILES_LOCATION:-$HOME/.dotfiles}"
SOPS_AGE_SYSTEM_FILE="/var/lib/sops-nix/system-keys.txt"

info() { printf '[ \033[00;34m  \033[0m ] %s\n' "$1"; }
error() { printf '[\033[0;31mERROR\033[0m] %s\n' "$1" 1>&2; }
fatal() { printf '[\033[0;31mFAIL\033[0m] %s\n' "$1" 1>&2; exit 1; }

check_bitwarden_unlocked() {
  if [ -z "$BW_SESSION" ]; then
    BW_SESSION="$(bw-session session "${bw_email}")"
    export BW_SESSION
  fi

  bw unlock --check > /dev/null || fatal "Vault must be unlocked"
}

export_sops_private_key() {
  host="$1"
  folder="$2"

  if yq '.keys[] | anchor' < "${DOTFILES_LOCATION}"/.sops.yaml | grep -E "^${host}-system$" > /dev/null;
    info "$host - has sops keys set. Fetching them"
    bw-session get-item-field "system-nixos-${host}" "sops-system-private-key" > "${folder}/sops.private"
  fi
}

export_luke_keys() {
  host="$1"
  folder="$2"

  nix run .#bw-session -- get-item "system-nixos-${host}" \
    | jq -rc '.fields[] | select(.name | startswith("luks")) | .name' \
    | while read -r field
        nix run .#bw-session -- get-item-field "system-nixos-${host}" "$field" > "${folder}/$field.key"
      done
}

remote_install() {
  host="$1"
  bw_email="$2"
  ssh_host="$3"
  shift 3

  ! test -d "${DOTFILES_LOCATION}" && fatal "dotfiles folder not found: ${DOTFILES_LOCATION}"
  ! test -d "${DOTFILES_LOCATION}/hosts/${host}" && fatal "No matching '${host}' under '${DOTFILES_LOCATION}/hosts'"

  info "'${host}' contains system wide private keys. Getting private keys from Bitwarden"
  extra_files="$(mktemp -d)"
  fetch_age_system_private_key "$host" > "${extra_files}/${SOPS_AGE_SYSTEM_FILE}"

  nix run github:nix-community/nixos-anywhere -- \
    --extra-files "$extra_files" \
    --flake ".#${host}" \
    "$@" \
    "${ssh_host}"
}

case "$1" in
  --help) usage && exit 0         ;;
  remote) remote_install "$@"     ;;
  local)  local_install "$@"      ;;
esac


