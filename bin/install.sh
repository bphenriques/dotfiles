#!/usr/bin/env sh
set -ef

# Constants
DOTFILES_LOCATION="$HOME"/.dotfiles
HOST_FILE_LOCATION="$DOTFILES_LOCATION/.nix-host"
MACOS_HOST_REGEX="^.*-(macos|darwin)$"

SSH_KEY_EMAIL_ADDRESS="4727729+bphenriques@users.noreply.github.com"
SSH_KEY_LOCATION="$HOME"/.ssh/id_ed25519.pub

usage() {
  echo "install.sh [nixos-install|darwin-install|post-install]
"
}

info() { printf '[ \033[00;34m..\033[0m ] %s\n' "$1"; }
success() { printf '[ \033[00;32mOK\033[0m ] %s\n' "$1"; }
warn() { printf '[ \033[01;33mWARN\033[0m ] %s\n' "$1"; }
error() { printf '[\033[0;31mERROR\033[0m] %s\n' "$1" 1>&2; }
fatal() { printf '[\033[0;31mFAIL\033[0m] %s\n' "$1" 1>&2; exit 1; }
press_to_continue() { info 'Press any key to continue'; read -r _; }
append_if_absent() { touch "$2"; grep --quiet --fixed-strings -- "$1" "$2" || echo "$1" >> "$2"; }

available_hosts() { find "$DOTFILES_LOCATION"/host/ -mindepth 1 -maxdepth 1 -type d -exec basename {} \;; }
available_unix_hosts() { available_hosts | grep -vE "${MACOS_HOST_REGEX}"; }
available_darwin_hosts() { available_hosts | grep -E "${MACOS_HOST_REGEX}"; }

install_homebrew() {
  if ! command -v brew >/dev/null; then
    if ! xcode-select -p >/dev/null; then
      info 'Homebrew - Installing XCode commandline tools'
      xcode-select --install
    fi

    info 'Homebrew - Installing...'
    /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
  fi
  success 'Homebrew - Installed!'
}

install_nix_darwin() {
  if ! command -v /run/current-system/sw/bin/darwin-rebuild >/dev/null; then
    info 'Nix Darwin - Installing nix-darwin...'
    nix-build https://github.com/LnL7/nix-darwin/archive/master.tar.gz -A installer
    ./result/bin/darwin-installer
  fi
  success 'Nix Darwin - Installed!'
}

clone_dotfiles() {
  if ! test -d "$DOTFILES_LOCATION" || (find "$DOTFILES_LOCATION" -maxdepth 0 -empty | read -r _); then
    info 'dotfiles - Cloning!'
    git clone -b master git@github.com:bphenriques/dotfiles.git "$DOTFILES_LOCATION"
  fi
  success 'dotfiles - Cloned!'
}

setup_ssh() {
  if [ ! -f "$SSH_KEY_LOCATION" ]; then
    info 'SSH Key - Creating new key'
    ssh-keygen -t ed25519 -C "$SSH_KEY_EMAIL_ADDRESS"
    info "SSH Key - Copy the public key below to https://github.com/settings/ssh/new"
    cat "$SSH_KEY_LOCATION"
    echo ''
    press_to_continue
  fi
  success 'SSH Key - Created!'
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

ensure_bitwarden_login() {
  bw login --check >/dev/null   || fail "You are not logged in: bw login"
  bw unlock --check >/dev/null  || fail "The vault is locked: bw unlock"
}

fetch_bitwarden_private_key() {
  bw get item "$1" | jq --raw-output '.fields[] | select(.name=="private-key") | .value'
}

command -v nix >/dev/null || fatal 'Nix is not installed (https://nixos.org/manual/nix/stable/#chap-installation)'
command -v git >/dev/null || fatal 'Git is not installed.'

nixos-install --host ... [--ssh-host]|

case "$1" in
  nixos-install)
    while [ $# -gt 0 ]; do
      case "$1" in
      --host)     host="$2";      shift 2 ;;
      --ssh-host) ssh_host="$2";  shift 2 ;;
      *)          break                   ;;
      esac
    done
    test -z "${host}"     && error "host is not set!"     && usage && exit 1
    test -z "${ssh_host}" && error "ssh_host is not set!" && usage && exit 1

    if ! available_unix_hosts | grep "^$host$" > /dev/null; then
      echo "Available nixos hosts:" && available_unix_hosts | xargs -I{} echo "- {}"
      fatal "Unrecognized nixos host '${host}'."
    fi

    exit 0

    temp="$(mktemp -d)"
    if ! "$SCRIPT_PATH"/init-keys.sh "$temp/$HOME_DIRECTORY"; then
      fatal "Failed to initialize keys."
    fi

    # TODO: Arguments: --secret {id}
    # /persist/config/bphenriques/home/bphenriques


    nix build ".#nixosConfigurations.laptop.config.system.build.toplevel" --show-trace
    nix run github:nix-community/nixos-anywhere -- --extra-files "$temp" --flake "${TARGET}" "${HOST}"
    ;;
  darwin-install)
    shift 1
    while [ $# -gt 0 ]; do
      case "$1" in
      --host) host="$2"; shift 2 ;;
      *)      break ;;
      esac
    done
    test -z "${host}" && error "host is not set!" && usage && exit 1

    if [ "$(uname -s)" != "Darwin" ]; then
      fatal "Running darwin-install from a incompatible operating system: $(uname -s)"
    elif ! available_darwin_hosts | grep "^$host$" > /dev/null; then
      echo "Available darwin hosts:" && available_darwin_hosts | xargs -I{} echo "- {}"
      fatal "Unrecognized darwin host '${host}'."
    elif [ -f "$HOST_FILE_LOCATION" ] && [ "$(cat "$HOST_FILE_LOCATION")" != "${host}" ]; then
      fatal "There is already a host set that is not identical. Delete ${HOST_FILE_LOCATION} and try again."
    fi

    install_nix_darwin
    install_homebrew
    setup_ssh
    clone_dotfiles
    ;;
  post-install)
    shift 1
    setup_ssh
    clone_dotfiles
    select_host
    ;;
  *)
    usage
    fatal "Invalid argument!"
    ;;
esac
