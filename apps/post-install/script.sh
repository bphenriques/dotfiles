#shellcheck shell=bash

DOTFILES_LOCATION="${DOTFILES_LOCATION:-"$HOME"/.dotfiles}"
DOTFILES_PRIVATE_LOCATION="${DOTFILES_LOCATION}-private"
HOST_FILE_LOCATION="$DOTFILES_LOCATION/.nix-host"
SOPS_AGE_KEY_FILE="${SOPS_AGE_KEY_FILE:-"$HOME/.config/sops/age/keys.txt"}"

usage() { echo "Required arguments: <host> <bitwarden-email>"; }
info() { printf '[ .. ] %s\n' "$1"; }
success() { printf '[ OK ] %s\n' "$1"; }
fatal() { printf '[FAIL] %s\n' "$1" 1>&2; exit 1; }
append_if_absent() {
  line="$1"
  file="$2"

  touch "$file"
  grep --quiet --fixed-strings -- "$line" "$file" || echo "$line" >> "$file"
}

clone_dotfiles() {
  if ! test -d "${DOTFILES_LOCATION}" || (find "${DOTFILES_LOCATION}" -maxdepth 0 -empty | read -r _); then
    info "dotfiles - Cloning to ${DOTFILES_LOCATION}"
    git clone git@github.com:bphenriques/dotfiles.git "$DOTFILES_LOCATION"
  else
    success "dotfiles - available in ${DOTFILES_LOCATION}"
  fi

  if ! test -d "${DOTFILES_PRIVATE_LOCATION}" || (find "${DOTFILES_PRIVATE_LOCATION}" -maxdepth 0 -empty | read -r _); then
    info "dotfiles-private - Cloning to ${DOTFILES_PRIVATE_LOCATION}"
    git clone git@github.com:bphenriques/dotfiles-private.git "$DOTFILES_PRIVATE_LOCATION"
  else
    success "dotfiles-private - available in ${DOTFILES_PRIVATE_LOCATION}"
  fi
}

setup_ssh() {
  host="$1"

  test ! -d "$HOME"/.ssh && mkdir -m 700 "$HOME"/.ssh
  info "SSH Key - Fetching private key"
  dotfiles-secrets fetch ssh-private-key "$host" > /tmp/.ssh_id_ed25519
  chmod 600 /tmp/.ssh_id_ed25519
  mv /tmp/.ssh_id_ed25519 "$HOME"/.ssh/id_ed25519

  info "SSH Key - Deriving public key from the private one"
  ssh-keygen -f "$HOME"/.ssh/id_ed25519 -y > "$HOME"/.ssh/id_ed25519.pub
  chmod 644 /tmp/.ssh_id_ed25519.pub
}

set_host() {
  host="$1"
  test -z "${host}" && fatal "host must not be empty!"
  ! test -d "${DOTFILES_LOCATION}/hosts/${host}" && fatal "No matching '${host}' under '${DOTFILES_LOCATION}/hosts'"

  if [ -f "$HOST_FILE_LOCATION" ] && [ "$(cat "$HOST_FILE_LOCATION")" != "${host}" ]; then
    fatal "There is already a host set that is not identical. Delete ${HOST_FILE_LOCATION} and try again."
  fi

  echo "${host}" > "${HOST_FILE_LOCATION}"
  success "Host - Set to ${host}!"
}

import_age_private_keys() {
  host="$1"

  info "Sops Private Key - Checking"
  if dotfiles-secrets exists sops-private-key "${host}"; then
    info "Fetching ${host} private sops key to ${SOPS_AGE_KEY_FILE}"
    mkdir -p "$(dirname "${SOPS_AGE_KEY_FILE}")"
    append_if_absent "$(dotfiles-secrets fetch sops-private-key "${host}")" > "${SOPS_AGE_KEY_FILE}"
    success "Sops - Set"
  else
    success "Sops Private Key - Not needed"
  fi
}

import_gpg() {
  host="$1"

  info "GPG - Importing!"
  dotfiles-secrets fetch gpg-private-key "$host" | gpg --import
  dotfiles-secrets fetch gpg-public-key "$host" | gpg --import
  success "GPG - Imported!"
}

# FIXME: I am not that bothered but I should point to the nixpkgs set in my flakes: https://github.com/NixOS/nix/issues/2982
set_root_nixpkgs_channel() {
  info "Nix Channels - Setting.."
  sudo -i nix-channel --add https://nixos.org/channels/nixpkgs-unstable nixpkgs
  sudo -i nix-channel --update nixpkgs
  success "Nix Channels - Set"
}

# FIXME: getting permission denied (despite being pulled already) in fresh installations. Sorted out by building once from my Git repo.
# https://discourse.nixos.org/t/nixos-rebuild-switch-fails-under-flakes-and-doas-with-git-warning-about-dubious-ownership/46069
build_once_fix_git_permissions() {
  info ".dotfiles - building once.."
  cd "$DOTFILES_LOCATION" || fatal "Failed to go to dotfiles directory"
  nix build ".#nixosConfigurations.$host.config.system.build.toplevel" --show-trace
  success ".dotfiles - done"
}

if [ "$1" = "--help" ] || [ $# -lt 2 ]; then
  usage
  exit
fi

host="$1"
bw_email="$2"

BW_SESSION="$(bw-session session "${bw_email}")"
export BW_SESSION

set_root_nixpkgs_channel

setup_ssh "${host}"
clone_dotfiles
set_host "${host}"
import_age_private_keys "${host}"
import_gpg "${host}"

build_once_fix_git_permissions
