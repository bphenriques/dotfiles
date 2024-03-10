#!/usr/bin/env sh
# shellcheck disable=SC2016,SC1091,SC1090,SC2002
#
# Bootstraps the system for the dotfiles.
#
set -euf

# Constants
XDG_CONFIG_HOME=${XDG_CONFIG_HOME:-"$HOME"/.config} # Set if absent.

DOTFILES_LOCATION="$HOME"/.dotfiles
HOST_FILE_LOCATION="$DOTFILES_LOCATION/.nix-host"

SSH_KEY_EMAIL_ADDRESS="4727729+bphenriques@users.noreply.github.com"
SSH_KEY_LOCATION="$HOME"/.ssh/id_ed25519.pub
SSH_TYPE=ed25519

# Utility methods
info() {
  # shellcheck disable=SC2059
  printf '\r  [ \033[00;34m..\033[0m ] %s\n' "$1"
}

success() {
  # shellcheck disable=SC2059
  printf '\r\033[2K  [ \033[00;32mOK\033[0m ] %s\n' "$1"
}

warn() {
  # shellcheck disable=SC2059
  printf '\r\033[2K  [ \033[01;33mWARN\033[0m ] %s\n' "$1"
}

fail() {
  # shellcheck disable=SC2059
  printf '\r\033[2K  [\033[0;31mFAIL\033[0m] %s\n' "$1" 1>&2 # Redirect to stderror
  exit 1
}

press_to_continue() {
  info 'Press any key to continue'
  # shellcheck disable=SC2162
  read _
}

append_if_absent() {
  line="$1"
  file="$2"

  touch "$file"
  grep --quiet --fixed-strings -- "$line" "$file" || echo "$line" >> "$file"
}

check_requirements() {
  (command -v nix >/dev/null && success 'Nix - Installed!') || fail 'Nix is not installed. Install it manually: https://nixos.org/manual/nix/stable/#chap-installation'
  (command -v git >/dev/null && success 'Git - Installed!') || fail 'Git is not installed.'
}

install_nix_flakes() {
  info 'Nix Flakes - Checking...'
  if ! nix flake show templates >/dev/null; then
    if [ -d /etc/nixos ]; then
      info 'Nix Flakes - Encountered NixOS. Run sudo nano /etc/nixos/configuration.nix and update the file with:'
      echo 'nix.settings.experimental-features = [ "nix-command" "flakes" ];'
      echo ''
      echo 'Once done:'
      echo '1. sudo nixos-rebuild switch'
      echo '2. reboot'
      press_to_continue
      exit 1
    else
      info 'Nix Flakes - Installing...'
      # nix-env -iA nixpkgs.nixUnstable - Do I need this?
      mkdir -p "$XDG_CONFIG_HOME"/nix
      append_if_absent 'experimental-features = nix-command flakes' "$XDG_CONFIG_HOME"/nix/nix.conf
    fi
    success 'Nix Flakes - Installed!'
  else
    success 'Nix Flakes - Already available!'
  fi
}

install_homebrew() {
  info 'Homebrew - Checking...'
  if ! command -v brew >/dev/null; then
    if ! xcode-select -p >/dev/null; then
      info 'Homebrew - Installing XCode Commandline tools'
      xcode-select --install
    fi

    info 'Homebrew - Installing...'
    /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

    info 'Homebrew - Setting up PATH...'
    info 'Homebrew - Now follow Homebrew instruction above with extra instructions...'
    info 'Homebrew - Ensure that /etc/zprofile.orig content is restored to bring back path_helper'
    info 'Homebrew - Then add add the shellenv helper in /etc/zprofile and not ~/.zprofile'
    press_to_continue
    . /etc/zprofile
  fi
  success 'Homebrew - Installed!'
}

install_nix_darwin() {
  info 'Nix Darwin - Checking...'
  if ! command -v /run/current-system/sw/bin/darwin-rebuild >/dev/null; then
    info 'Nix Darwin - Installing nix-darwin as installing from flakes is not sufficient (say yes to everything)...'
    nix-build https://github.com/LnL7/nix-darwin/archive/master.tar.gz -A installer
    ./result/bin/darwin-installer
  fi
  success 'Nix Darwin - Installed!'
}

clone_default_repos() {
  info 'Cloning Repos - Checking...'
  if [ ! -d "$DOTFILES_LOCATION" ]; then
    info 'Cloning Repos - dotfiles...'
    # Unfortunately, can't create the hidden folder directly.
    tmp=$(mktemp -d)
    git clone git@github.com:bphenriques/dotfiles.git "$tmp" && mv "$tmp" "$DOTFILES_LOCATION"
  fi
  success 'Cloning Repos - finished!'
}

setup_ssh() {
  info "SSH Key - Checking if '$SSH_KEY_LOCATION' public key already exists..."
  if [ ! -f "$SSH_KEY_LOCATION" ]; then
    info 'SSH Key - Setting it up!'
    ssh-keygen -t $SSH_TYPE -C "$SSH_KEY_EMAIL_ADDRESS"

    case "$(uname -s)" in
        Darwin)
            cat "$SSH_KEY_LOCATION" | pbcopy
            open https://github.com/settings/ssh/new
            press_to_continue
            ;;
        *)
            info "SSH Key - Copy the following public key to https://github.com/settings/ssh/new"
            info "---------------"
            cat "$SSH_KEY_LOCATION"
            info "---------------"
            press_to_continue
            ;;
    esac
  fi
  success 'SSH Key - Done!'
}

select_host() {
  info 'Nix Host Type - Checking...'
  if [ ! -f "$HOST_FILE_LOCATION" ]; then
    printf "Available hosts:\n"
    find "$DOTFILES_LOCATION"/host/ -mindepth 1 -type d -print0 -exec basename {} \; | xargs -0 -I{} echo "- {}"
    printf "\n"
    while true; do
      printf "Introduce the host type: "
      read -r nix_host
      if [ -d "$DOTFILES_LOCATION/host/$nix_host" ]; then
        break
      fi
    done
    printf '%s' "$nix_host" > "$HOST_FILE_LOCATION"
  elif [ ! -d "$DOTFILES_LOCATION/host/$(cat "$HOST_FILE_LOCATION")" ]; then
    fail "Nix Host - Invalid host '$(cat "$HOST_FILE_LOCATION")'. Delete and try-again."
  fi
  success "Nix Host Type - Set to '$(cat "$HOST_FILE_LOCATION")'!"
}

setup_secrets() {
  info 'Sops secrets - Checking...'
  if [ ! -f "$HOME/.ssh/id_ed25519" ]; then
    fail "Sops secrets - No SSH key file: ~/.ssh/id_ed25519"
  else
    mkdir -p "$XDG_CONFIG_HOME"/sops/age

    if [ ! -f "$XDG_CONFIG_HOME/sops/age/keys.txt" ]; then
      nix-shell -p ssh-to-age --run "ssh-to-age -private-key -i \"$HOME/.ssh/id_ed25519\" > \"$XDG_CONFIG_HOME/sops/age/keys.txt\""
    else
      info "Sops secret already set at $XDG_CONFIG_HOME/sops/age/keys.txt"
    fi
  fi
  success "Sops secrets"
}

check_requirements

setup_ssh
setup_secrets
install_nix_flakes
case "$(uname -s)" in
    Darwin)
      install_nix_darwin
      install_homebrew
      ;;
    *)  ;;
esac
clone_default_repos
select_host

success 'Bootstrap - Complete!'
