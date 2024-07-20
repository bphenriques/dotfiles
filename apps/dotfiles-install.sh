#!/usr/bin/env sh

# Constants
DOTFILES_LOCATION="$HOME"/.dotfiles
SSH_KEY_LOCATION="$HOME"/.ssh/id_ed25519.pub
HOST_FILE_LOCATION="$DOTFILES_LOCATION/.nix-host"

usage() {
  echo "Setups the dotfiles repository under $DOTFILES_LOCATION

Usage: dotfiles-installer.sh --host <host>

  --host            matching directory name under  match on of the directories under $DOTFILES_LOCATION/hosts/

Options:
  --ssh-key-comment defaults to git user.email
"
}

info() { printf '[ \033[00;34m..\033[0m ] %s\n' "$1"; }
success() { printf '[ \033[00;32mOK\033[0m ] %s\n' "$1"; }
warn() { printf '[ \033[01;33mWARN\033[0m ] %s\n' "$1"; }
error() { printf '[\033[0;31mERROR\033[0m] %s\n' "$1" 1>&2; }
fatal() { printf '[\033[0;31mFAIL\033[0m] %s\n' "$1" 1>&2; exit 1; }
press_to_continue() { info 'Press any key to continue'; read -r _; }

clone_dotfiles() {
  # Works for impermanence where the directory was mounted but it is empty
  if ! test -d "$DOTFILES_LOCATION" || (find "$DOTFILES_LOCATION" -maxdepth 0 -empty | read -r _); then
    info 'dotfiles - Cloning!'
    git clone -b master git@github.com:bphenriques/dotfiles.git "$DOTFILES_LOCATION"
  fi
  success 'dotfiles - Cloned!'
}

setup_ssh() {
  local comment="$1"
  if test -z "$comment"; then
    fail "SSH comment is empty"
  fi

  if [ ! -f "$SSH_KEY_LOCATION" ]; then
    info 'SSH Key - Creating new key'
    ssh-keygen -t ed25519 -C "$comment"
    info "SSH Key - Copy the public key below to https://github.com/settings/ssh/new"
    cat "$SSH_KEY_LOCATION"
    echo ''
    press_to_continue
  fi
  success 'SSH Key - Created!'
}

validate_host() {
  host="$1"
  if test -z "${host}"; then
    fatal "host must not be empty!"
  elif ! test -d "${DOTFILES_LOCATION}/hosts/${host}"; then
    fatal "No matchist '${host}' under '${DOTFILES_LOCATION}/hosts'"
  elif [ -f "$HOST_FILE_LOCATION" ] && [ "$(cat "$HOST_FILE_LOCATION")" != "${host}" ]; then
    fatal "There is already a host set that is not identical. Delete ${HOST_FILE_LOCATION} and try again."
  fi
  success 'Host - Valid!'
}

setup_git_filter() {
  info 'Nix Evaluation Secrets - Checking...'
  if ! "$DOTFILES_LOCATION"/bin/git-secret-filter.sh doctor; then
    info "Nix Evaluation Secrets - Initialing and checking out to to smudge the secrets"
    "$DOTFILES_LOCATION"/bin/git-secret-filter.sh init
    git checkout master
  fi
  success "Nix Evaluation Secrets - Done!"
}

ssh_key_comment="$(git config user.email)"
host=
while [ $# -gt 0 ]; do
  case "$1" in
    --help)             usage;                  exit  0 ;;
    --ssh-key-comment)  ssh_key_comment="$2";   shift 2 ;;
    --host)             host="$2";              shift 2 ;;
    *) break ;;
  esac
done

setup_ssh "${ssh_key_comment}"
clone_dotfiles
validate_host "${host}"
