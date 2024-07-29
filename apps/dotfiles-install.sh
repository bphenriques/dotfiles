#!/usr/bin/env sh

usage() {
  echo "Setups the dotfiles.

Usage: dotfiles-installer.sh <host> [--dotfiles-location] [-ssh-directory] [--ssh-key-comment]

Arguments:
  host                name of the host under the hosts folder

Options:
  --dotfiles-location defaults to \$HOME/.dotfiles
  --ssh-directory     defaults to \$HOME/.ssh
  --ssh-key-comment   defaults to git user.email
"
}

info() { printf '[ \033[00;34m..\033[0m ] %s\n' "$1"; }
success() { printf '[ \033[00;32mOK\033[0m ] %s\n' "$1"; }
warn() { printf '[ \033[01;33mWARN\033[0m ] %s\n' "$1"; }
error() { printf '[\033[0;31mERROR\033[0m] %s\n' "$1" 1>&2; }
fatal() { printf '[\033[0;31mFAIL\033[0m] %s\n' "$1" 1>&2; exit 1; }
press_to_continue() { info 'Press any key to continue'; read -r _; }

clone_dotfiles() {
  location="$dotfiles_location"
  # Works for impermanence where the directory is mounted and empty
  if ! test -d "$location" || (find "$location" -maxdepth 0 -empty | read -r _); then
    info "dotfiles - Cloning to $location"
    git clone -b master git@github.com:bphenriques/dotfiles.git "$location"
  fi
  success "dotfiles - available in $location"
}

setup_ssh() {
  directory="$1"
  comment="$2"
  test -z "$comment"  && fatal "SSH comment is empty"

  echo "$directory"
  mkdir -p "${directory}"
  if [ ! -f "$directory"/id_ed25519.pub ]; then
    info "SSH Key - Exporting key-pair on $directory"
    ssh-keygen -t ed25519 -C "$comment" -f "$directory"/id_ed25519
    info 'SSH Key - Copy the public key below to https://github.com/settings/ssh/new'
    cat "$directory"/id_ed25519.pub
    echo ''
    press_to_continue
  else
    success "SSH Key - Key-pair already present in $directory"
  fi
  success "SSH Key - Key-pair available in $directory"
}

setup_git_filter() {
  info 'Nix Evaluation Secrets - Checking...'
  if ! "$dotfiles_location"/bin/git-secret-filter.sh doctor; then
    info "Nix Evaluation Secrets - Initialing and checking out to to smudge the secrets"
    "$dotfiles_location"/bin/git-secret-filter.sh init
    git checkout master
  fi
  success "Nix Evaluation Secrets - Done!"
}

validate_host() {
  dotfiles_location="$1"
  host="$2"
  host_file_location="$dotfiles_location/.nix-host"

  test -z "${host}" && fatal "host must not be empty!"
  ! test -d "${dotfiles_location}/hosts/${host}" && fatal "No matching '${host}' under '${dotfiles_location}/hosts'"
  if [ -f "$host_file_location" ] && [ "$(cat "$host_file_location")" != "${host}" ]; then
    fatal "There is already a host set that is not identical. Delete ${host_file_location} and try again."
  fi
  success 'Host - Valid!'
}

if [ "$1" == "--help" ]; then
  usage
  exit
fi

dotfiles_location="$HOME"/.dotfiles
ssh_directory="$HOME"/.ssh
ssh_key_comment="$(git config user.email)"

host="$1"
shift 1
while [ $# -gt 0 ]; do
  case "$1" in
    --dotfiles-location)  dotfiles_location="$2";   shift 2 ;;
    --ssh-directory)      ssh_directory="$2";       shift 2 ;;
    --ssh-key-comment)    ssh_key_comment="$2";     shift 2 ;;
    *) break ;;
  esac
done

setup_ssh "${ssh_directory}" "${ssh_key_comment}"
clone_dotfiles "${dotfiles_location}"
validate_host "${dotfiles_location}" "${host}"
