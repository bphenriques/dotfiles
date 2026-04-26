#shellcheck shell=bash
set -euo pipefail

BRANCH_NAME="${BRANCH_NAME:-main}"
DOTFILES_LOCATION="${DOTFILES_LOCATION:-"$HOME"/.dotfiles}"
DOTFILES_PRIVATE_LOCATION="${DOTFILES_LOCATION}-private"
HOST_FILE_LOCATION="$DOTFILES_LOCATION/.nix-host"
SOPS_AGE_KEY_FILE="${SOPS_AGE_KEY_FILE:-"$HOME/.config/sops/age/keys.txt"}"

fatal() {
  printf '[FAIL] %s\n' "$1" >&2
  exit 1
}
info() { printf '[ .. ] %s\n' "$1"; }
success() { printf '[ OK ] %s\n' "$1"; }

usage() { echo "Usage: $0 <host> <bitwarden-email>" && exit 1; }

clone_dotfiles() {
  if [ ! -d "$DOTFILES_LOCATION" ] || [ -z "$(ls -A "$DOTFILES_LOCATION" 2>/dev/null)" ]; then
    info "dotfiles - Cloning to ${DOTFILES_LOCATION}"
    git clone git@github.com:bphenriques/dotfiles.git "$DOTFILES_LOCATION"
    cd "$DOTFILES_LOCATION"
    git checkout "$BRANCH_NAME"
  else
    success "dotfiles - available in ${DOTFILES_LOCATION}"
  fi

  if [ ! -d "$DOTFILES_PRIVATE_LOCATION" ] || [ -z "$(ls -A "$DOTFILES_PRIVATE_LOCATION" 2>/dev/null)" ]; then
    info "dotfiles-private - Cloning to ${DOTFILES_PRIVATE_LOCATION}"
    git clone git@github.com:bphenriques/dotfiles-private.git "$DOTFILES_PRIVATE_LOCATION"
  else
    success "dotfiles-private - available in ${DOTFILES_PRIVATE_LOCATION}"
  fi
}

set_host() {
  local host="$1"
  test -z "${host}" && fatal "host must not be empty!"
  ! test -d "${DOTFILES_LOCATION}/hosts/${host}" && fatal "No matching '${host}' under '${DOTFILES_LOCATION}/hosts'"

  if [ -f "$HOST_FILE_LOCATION" ] && [ "$(cat "$HOST_FILE_LOCATION")" != "${host}" ]; then
    fatal "There is already a host set that is not identical. Delete ${HOST_FILE_LOCATION} and try again."
  fi

  echo "${host}" >"${HOST_FILE_LOCATION}"
  success "Host - Set to ${host}!"
}

setup_ssh_key() {
  if [ ! -f "$HOME/.ssh/id_ed25519" ]; then
    info "SSH Key - Generating new key pair (you will be prompted for a passphrase)..."
    mkdir -p "$HOME/.ssh"
    chmod 700 "$HOME/.ssh"
    ssh-keygen -t ed25519 -a 100 -f "$HOME/.ssh/id_ed25519" -C "${USER}@$(hostname)"

    success "SSH Key - Generated at ~/.ssh/id_ed25519"
    echo ""
    echo "Public key (add to GitHub/servers):"
    cat "$HOME/.ssh/id_ed25519.pub"
    echo ""
  else
    info "SSH Key - Already exists. Skipping."
  fi
}

import_age_private_key() {
  local host="$1"

  info "Sops Private Key - Fetching for ${host}..."
  local key
  key="$(dotfiles-secrets "$bw_email" fetch sops-private-key "${host}")" || fatal "Failed to fetch sops-private-key for ${host}"

  # Append key if not already present (idempotent)
  mkdir -p "$(dirname "$SOPS_AGE_KEY_FILE")"
  grep --quiet --fixed-strings -- "$key" "$SOPS_AGE_KEY_FILE" 2>/dev/null || echo "$key" >>"$SOPS_AGE_KEY_FILE"
  chmod 600 "$SOPS_AGE_KEY_FILE"

  success "Sops - Added to ${SOPS_AGE_KEY_FILE}"
}

import_gpg() {
  info "GPG - Importing..."
  dotfiles-secrets "$bw_email" fetch gpg-private-key | gpg --batch --yes --import || fatal "Failed to import GPG private key"
  dotfiles-secrets "$bw_email" fetch gpg-public-key | gpg --batch --yes --import || fatal "Failed to import GPG public key"
  success "GPG - Imported!"
}

if [ "$1" = "--help" ] || [ $# -lt 2 ]; then
  usage
fi

host="$1"
bw_email="$2"

info "Unlocking Bitwarden account (${bw_email})..."
BW_SESSION="$(bw-session session "${bw_email}")"
export BW_SESSION

setup_ssh_key
clone_dotfiles
set_host "${host}"
import_age_private_key "${host}"
import_gpg

success "Post-install complete!"
