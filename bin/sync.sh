#!/usr/bin/env sh
# shellcheck disable=SC1091
set -euf
SCRIPT_PATH="$(dirname "$0")"
# shellcheck source=util.sh
. "$SCRIPT_PATH"/util.sh

XDG_CONFIG_HOME=${XDG_CONFIG_HOME:-"$HOME"/.config} # Set if absent.

sync_flake() {
  local host_target
  host_target="$(cat "$HOST_FILE_LOCATION")"
  if [ -d /etc/nixos ]; then
    info "Syncing NixOS host '$host_target'.."
    sudo nixos-rebuild switch --flake ".#$host_target"
  elif [ "$(uname)" = "Darwin" ]; then
      info "Syncing MacOS host '$host_target'.."
    nix build ".#darwinConfigurations.$host_target.system" $extra_args
    ./result/sw/bin/darwin-rebuild switch --flake ".#$host_target" #--show-trace --no-eval-cache
  else
    fail "Unsupported Operating System: $(uname)"
  fi
}

# Check type of host.
HOST_FILE_LOCATION="$HOME/.dotfiles/.nix-host"
[ ! -f "$HOST_FILE_LOCATION" ] && fail "$HOST_FILE_LOCATION not found"

sync_flake
