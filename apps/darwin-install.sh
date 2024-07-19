#!/usr/bin/env sh

usage() {
  echo "Setups the nix darwin

Usage: darwin-install.sh
"
}

info() { printf '[ \033[00;34m..\033[0m ] %s\n' "$1"; }
success() { printf '[ \033[00;32mOK\033[0m ] %s\n' "$1"; }
fatal() { printf '[\033[0;31mFAIL\033[0m] %s\n' "$1" 1>&2; exit 1; }

install_nix_darwin() {
  if ! command -v /run/current-system/sw/bin/darwin-rebuild >/dev/null; then
    info 'Nix Darwin - Installing nix-darwin...'
    nix-build https://github.com/LnL7/nix-darwin/archive/master.tar.gz -A installer
    ./result/bin/darwin-installer
  fi
  success 'Nix Darwin - Installed!'
}

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

install_nix_darwin
install_homebrew
