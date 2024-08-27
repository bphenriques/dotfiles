#!/usr/bin/env sh

BRANCH_NAME=add-laptop #FIXME: Remove after we sort-out everything
DOTFILES_LOCATION="${DOTFILES_LOCATION:-"$HOME"/.dotfiles}"
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
    git clone -b "${BRANCH_NAME}" git@github.com:bphenriques/dotfiles.git "$DOTFILES_LOCATION"
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

init_sops_git_filter() {
  host="$1"

  info "Sops Git Filter - Checking.."
  if ! "${DOTFILES_LOCATION}"/bin/sops-git-filter.sh check "${host}"; then
    info "Sops Git Filter - Setting up for '${host}'"
    "${DOTFILES_LOCATION}"/bin/sops-git-filter.sh init "${host}"
    success "Sops Git Filter - Set for '${host}'"
  else
    success "Sops Git Filter - Already set for '${host}'"
  fi
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
init_sops_git_filter "${host}"
