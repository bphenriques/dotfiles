#!/usr/bin/env sh

DOTFILES_LOCATION="${DOTFILES_LOCATION:-"$HOME"/.dotfiles}"
DOTFILES_PRIVATE_LOCATION="${DOTFILES_LOCATION}-private"
HOST_FILE_LOCATION="$DOTFILES_LOCATION/.nix-host"
SOPS_AGE_KEY_FILE="${SOPS_AGE_KEY_FILE:-"$HOME/.config/sops/age/keys.txt"}"

usage() {
  echo "dotfiles-install.sh <host> <bitwarden-email>"
}

info() { printf '[ \033[00;34m  \033[0m ] %s\n' "$1"; }
success() { printf '[ \033[00;32mOK\033[0m ] %s\n' "$1"; }
fatal() { printf '[\033[0;31mFAIL\033[0m] %s\n' "$1" 1>&2; exit 1; }
press_to_continue() { info 'Press any key to continue'; read -r _; }
append_if_absent() {
  line="$1"
  file="$2"

  touch "$file"
  grep --quiet --fixed-strings -- "$line" "$file" || echo "$line" >> "$file"
}

clone_dotfiles() {
  # Works for impermanence where the directory is mounted but empty
  if ! test -d "${DOTFILES_LOCATION}" || (find "${DOTFILES_LOCATION}" -maxdepth 0 -empty | read -r _); then
    info "dotfiles - Cloning to ${DOTFILES_LOCATION}"
    git clone git@github.com:bphenriques/dotfiles.git "$DOTFILES_LOCATION"
  fi
  if ! test -d "${DOTFILES_PRIVATE_LOCATION}" || (find "${DOTFILES_PRIVATE_LOCATION}" -maxdepth 0 -empty | read -r _); then
    info "dotfiles-private - Cloning to ${DOTFILES_PRIVATE_LOCATION}"
    git clone git@github.com:bphenriques/dotfiles-private.git "$DOTFILES_PRIVATE_LOCATION"
  fi

  success "dotfiles - available in ${DOTFILES_LOCATION}"
}

setup_ssh() {
  mkdir -p "$HOME"/.ssh
  if [ ! -f "$HOME"/.ssh/id_ed25519.pub ]; then
    ssh-keygen -t ed25519 -C "$(git config user.email)" -f "$HOME"/.ssh/id_ed25519
    info 'SSH Key - Copy the public key below to https://github.com/settings/ssh/new'
    cat "$HOME"/.ssh/id_ed25519.pub
    echo ''
    press_to_continue
  else
    success "SSH Key - Key-pair already present"
  fi
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
  bw unlock --check > /dev/null || fatal "Vault must be unlocked"

  mkdir -p "$(dirname "${SOPS_AGE_KEY_FILE}")"
  for secret in $(yq '.keys[] | anchor' < "${DOTFILES_LOCATION}"/.sops.yaml | grep -E "^${host}"); do
    info "Adding '${secret}' to ${SOPS_AGE_KEY_FILE}"
    value="$(bw get item "sops-age-key-${secret}" | jq --raw-output '.fields[] | select(.name=="private") | .value')"
    # shellcheck disable=SC2181
    if [ $? -ne 0 ]; then
      fatal "Can't find Bitwarden secret sops-age-key-${secret}'"
    fi
    append_if_absent "${value}" "${SOPS_AGE_KEY_FILE}"
  done
}

import_gpg() {
  info "GPG - Importing!"
  bw get item "github-gpg-private" | jq --raw-output '.notes' | gpg --import
  bw get item "github-gpg-public" | jq --raw-output '.notes' | gpg --import
  success "GPG - Imported!"
}

# https://github.com/NixOS/nix/issues/2982
set_root_nixpkgs_channel() {
  info "Nix Channels - Setting.."
  sudo -i nix-channel --add https://nixos.org/channels/nixpkgs-unstable nixpkgs
  sudo -i nix-channel --update nixpkgs
  success "Nix Channels - Set"
}

# e.g. that does not work out of the box which leads to Permission denied (despite being pulled already): sudo nixos-rebuild switch --flake \".#$host\"
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

BW_SESSION="$(bw-session "${bw_email}")"
export BW_SESSION

set_root_nixpkgs_channel

setup_ssh
clone_dotfiles
set_host "${host}"
import_age_private_keys "${host}"
import_gpg

build_once_fix_git_permissions
