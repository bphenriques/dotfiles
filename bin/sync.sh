#!/usr/bin/env sh
# shellcheck disable=SC1091
set -ef

XDG_CONFIG_HOME=${XDG_CONFIG_HOME:-"$HOME"/.config} # Set if absent.

info() { printf '[ \033[00;34m..\033[0m ] %s\n' "$1"; }
success() { printf '[ \033[00;32mOK\033[0m ] %s\n' "$1"; }
warn() { printf '[ \033[01;33mWARN\033[0m ] %s\n' "$1"; }
error() { printf '[\033[0;31mERROR\033[0m] %s\n' "$1" 1>&2; }
fatal() { printf '[\033[0;31mFAIL\033[0m] %s\n' "$1" 1>&2; exit 1; }

sync_flake() {
  local host_target
  host_target="$(cat "$HOST_FILE_LOCATION")"
  if [ -d /etc/nixos ]; then
    info "Syncing NixOS host '$host_target'.."
    sudo nixos-rebuild switch --flake ".#$host_target"
  elif [ "$(uname)" = "Darwin" ]; then
    info "Syncing MacOS host '$host_target'.."
    nix build --extra-experimental-features 'nix-command flakes' ".#darwinConfigurations.$host_target.system"
    ./result/sw/bin/darwin-rebuild switch --flake ".#$host_target" #--show-trace --no-eval-cache
  else
    fail "Unsupported Operating System: $(uname)"
  fi
}

# Check type of host.
HOST_FILE_LOCATION="$HOME/.dotfiles/.nix-host"
[ ! -f "$HOST_FILE_LOCATION" ] && fail "$HOST_FILE_LOCATION not found"

sync_flake
