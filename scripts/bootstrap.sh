#!/bin/sh
# shellcheck disable=SC2016,SC1091,SC1090
#
# Bootstraps the required dependencies for non-NixOS operating systems.
#
set -euf 
SCRIPT_PATH="$(dirname "$0")"
# shellcheck source=util.sh
. "$SCRIPT_PATH"/util.sh

XDG_CONFIG_HOME=${XDG_CONFIG_HOME:-"$HOME"/.config} # Set if absent.
WORKSPACE="$HOME/workspace"

install_nix() {
    info 'Nix - Checking...'
    if ! command -v nix > /dev/null; then
        info 'Nix - Install it manually: https://nixos.org/manual/nix/stable/#chap-installation'
        info 'Nix - Setting up "$ZDOTDIR/.zprofile'
        append_if_absent 'test -f "$HOME"/.nix-profile/etc/profile.d/nix.sh && . "$HOME"/.nix-profile/etc/profile.d/nix.sh' "$ZDOTDIR"/.zprofile
        info 'Nix - Press any key to continue.' && read -r
        . "$ZDOTDIR"/.zprofile
    fi
    success 'Nix - Installed!'
}

install_nix_flakes() {
    info 'Nix Flakes - Checking...'
    if ! nix flake check 2>/dev/null; then 
        info 'Nix Flakes - Installing...'
        nix-env -iA nixpkgs.nixFlakes
        mkdir -p "$XDG_CONFIG_HOME"/nix && touch "$XDG_CONFIG_HOME"/nix/nix.conf
        append_if_absent 'experimental-features = nix-command flakes' "$XDG_CONFIG_HOME"/nix/nix.conf
    fi
    success 'Nix Flakes - Installed!'
}

install_homebrew() {
    info 'Homebrew - Checking...'
    if ! command -v brew > /dev/null; then
        info 'Homebrew - Installing...'

        if ! xcode-select -p > /dev/null; then
           info 'Homebrew - Installing XCode Commandline tools'
           xcode-select --install
        fi
        /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
    fi
    success 'Homebrew - Installed!'
}

install_nix_darwin() {
    info 'Nix Darwin - Checking...'
    if ! command -v /run/current-system/sw/bin/darwin-rebuild > /dev/null; then
        info 'Nix Darwin - Installing nix-darwin...'
        nix-build https://github.com/LnL7/nix-darwin/archive/master.tar.gz -A installer
        ./result/bin/darwin-installer
    fi
    success 'Nix Darwin - Installed!'
}

clone_default_repos() {
    info 'Cloning Repos - Checking...'
    if [ ! -d "$WORKSPACE/knowledge-base" ]; then
        info 'Cloning Repos - Knowledge Base...'
        git clone git@github.com:bphenriques/knowledge-base.git "$WORKSPACE/knowledge-base"
    fi
    success 'Cloning Repos - finished!'
}

install_nix
install_nix_flakes
case "$(uname -s)" in
    Darwin)     install_nix_darwin
                install_homebrew
                ;;
   *)           ;;
esac
clone_default_repos

success 'Bootstrap - Complete! Restart your terminal!' && read -r
