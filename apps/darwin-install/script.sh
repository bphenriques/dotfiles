#shellcheck shell=bash

info() { printf '[ .. ] %s\n' "$1"; }
success() { printf '[ OK ] %s\n' "$1"; }

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
