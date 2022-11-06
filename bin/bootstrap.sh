#!/bin/sh
# shellcheck disable=SC2016,SC1091,SC1090,SC2002
#
# Bootstraps the required dependencies for non-NixOS operating systems.
#
set -euf

XDG_CONFIG_HOME=${XDG_CONFIG_HOME:-"$HOME"/.config} # Set if absent.

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

  # Append if absent
  touch "$file"
  grep --quiet --fixed-strings -- "$line" "$file" || echo "$line" >>"$file"
}

WORKSPACE="$HOME/workspace"
DOTFILES_LOCATION="$HOME"/.dotfiles
HOST_FILE_LOCATION="$DOTFILES_LOCATION/.nix-host"

SSH_KEY_EMAIL_ADDRESS="4727729+bphenriques@users.noreply.github.com"
SSH_KEY_LOCATION="$HOME"/.ssh/id_ed25519.pub
SSH_TYPE=ed25519

check_requirements() {
  (command -v nix >/dev/null  && success 'Nix - Installed!') || fail 'Nix is not installed. Install it manually: https://nixos.org/manual/nix/stable/#chap-installation'
}

install_nix_flakes() {
  info 'Nix Flakes - Checking...'
  if ! nix flake show templates >/dev/null; then
    info 'Nix Flakes - Installing...'
    nix-env -iA nixpkgs.nixUnstable
    mkdir -p "$XDG_CONFIG_HOME"/nix
    append_if_absent 'experimental-features = nix-command flakes' "$XDG_CONFIG_HOME"/nix/nix.conf
  fi
  success 'Nix Flakes - Installed!'
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
  if [ ! -d "$WORKSPACE/knowledge-base" ]; then
    info 'Cloning Repos - Knowledge Base...'
    git clone --recurse-submodules git@github.com:bphenriques/knowledge-base.git "$WORKSPACE/knowledge-base"
  fi

  if [ ! -d "$WORKSPACE/journal" ]; then
    info 'Cloning Repos - journal...'
    git clone git@github.com:bphenriques/journal.git "$WORKSPACE/journal"
  fi

  if [ ! -d "$DOTFILES_LOCATION" ]; then
    info 'Cloning Repos - dotfiles...'
    # Unfortunately, can't create the hidden folder directly.
    tmp=$(mktemp -d)
    git clone git@github.com:bphenriques/dotfiles.git "$tmp" && mv "$tmp" "$DOTFILES_LOCATION"
  fi
  success 'Cloning Repos - finished!'
}

normalize_command_names() {
  case "$(uname -s)" in
    Linux*)
            alias pbcopy='xsel — clipboard — input'
            alias open='xdg-open'
            ;;
      *)    ;;
  esac
}

setup_ssh() {
  info 'SSH Key - Checking...'
  if [ ! -f "$SSH_KEY_LOCATION" ]; then
    info 'SSH Key - Setting it up!'
    ssh-keygen -t $SSH_TYPE -C "$SSH_KEY_EMAIL_ADDRESS"
    (cat "$SSH_KEY_LOCATION" | pbcopy) && open https://github.com/settings/ssh/new && press_to_continue
  fi
  success 'SSH Key - Done!'
  press_to_continue
}

select_host() {
  info 'Nix Host Type - Checking...'

  if [ ! -f "$HOST_FILE_LOCATION" ]; then
    info 'Nix Host Type - Setting up...'
    nix_host=

    # shellcheck disable=SC2039
    select nix_host in $(find "$DOTFILES_LOCATION/hosts/" -type f -name '*.nix' -print0 | xargs -0 -I % basename % .nix); do
       test -n "$nix_host" && break
       warn "Invalid host!"
    done
    printf '%s' "$nix_host" >>"$HOST_FILE_LOCATION"
  fi
  success "Nix Host Type - Set to '$(cat "$HOST_FILE_LOCATION")'!"
}

misc_macos() {
  info 'MacOS - Creating screenshots directory'
  mkdir -pv "$HOME"/Pictures/screenshots
}

check_requirements

normalize_command_names
setup_ssh
install_nix_flakes
case "$(uname -s)" in
    Darwin)
                install_nix_darwin
                install_homebrew
                misc_macos
                ;;
    *)          ;;
esac
clone_default_repos
select_host

success 'Bootstrap - Complete! Restart your terminal!'
