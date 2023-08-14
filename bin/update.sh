#!/usr/bin/env sh
# shellcheck disable=SC1091
set -euf
SCRIPT_PATH="$(dirname "$0")"
# shellcheck source=util.sh
. "$SCRIPT_PATH"/util.sh

info 'Nix Flake - Upgrading...'
nix flake update
success 'Nix Flake - Complete!'

case "$(uname -s)" in
  Darwin)
    info 'Homebrew - Upgrading...'
    if [ "$(uname -m)" = "arm64" ]; then
      info "Homebrew - Using arm64 brew"
      brew_bin="/opt/homebrew/bin/brew"
    else
      info "Homebrew - Using x86 brew"
      brew_bin="/usr/local/bin/brew"
    fi
    $brew_bin upgrade && $brew_bin update
    success 'Homebrew - Complete!'
    ;;
  *) ;;
esac
