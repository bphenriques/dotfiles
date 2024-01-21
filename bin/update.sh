#!/usr/bin/env sh
# shellcheck disable=SC1091
set -euf
SCRIPT_PATH="$(dirname "$0")"
# shellcheck source=util.sh
. "$SCRIPT_PATH"/util.sh

update_flake() {
  info 'Nix Flake - Upgrading...'
  nix flake update
  success 'Nix Flake - Complete!'
}

update_homebrew() {
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
}

update_nix_darwin_multiuser() {
  info 'Nix Multiuser - Upgrading...'
  sudo -i sh -c 'nix-channel --update && nix-env --install --attr nixpkgs.nix && launchctl remove org.nixos.nix-daemon && launchctl load /Library/LaunchDaemons/org.nixos.nix-daemon.plist'
  success 'Nix Multiuser - Complete!'
}

# FIXME: https://nixos.org/manual/nix/stable/installation/upgrading#upgrading-nix
update_nix_nixos() {
  warn "Not supported yet. Need to figure out single-vs-multi user"
}

update_flake
case "$(uname -s)" in
  Darwin)
    update_homebrew
    update_nix_darwin_multiuser
    ;;
  *)
    if [ -d /etc/nixos ]; then
      update_nix_nioxs
    fi
    ;;
esac
