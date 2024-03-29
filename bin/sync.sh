#!/usr/bin/env sh
# shellcheck disable=SC1091
set -euf
SCRIPT_PATH="$(dirname "$0")"
# shellcheck source=util.sh
. "$SCRIPT_PATH"/util.sh

DEBUG=${DEBUG:-0}
DISABLE_CACHE=${DISABLE_CACHE:-0}

# Check type of host.
HOST_FILE_LOCATION="$HOME/.dotfiles/.nix-host"
[ ! -f "$HOST_FILE_LOCATION" ] && fail "$HOST_FILE_LOCATION not found"
HOST_TARGET=$(cat "$HOST_FILE_LOCATION")

XDG_CONFIG_HOME=${XDG_CONFIG_HOME:-"$HOME"/.config} # Set if absent.

sync_flake() {
  info "Syncing Host '$HOST_TARGET'.."
  extra_args=""
  if [ "$DEBUG" -ne 0 ]; then
    info "Enabling debug..."
    extra_args="--show-trace"
  fi
  if [ "$DISABLE_CACHE" -ne 0 ] && [ -d /etc/nixos ]; then
    info "Disabling cache..."
    extra_args="$extra_args --no-eval-cache"
  fi

  if [ -d /etc/nixos ]; then
    sudo nixos-rebuild switch --flake ".#$HOST_TARGET"
  elif [ "$(uname)" = "Darwin" ]; then
    nix build ".#darwinConfigurations.$HOST_TARGET.system" $extra_args
    ./result/sw/bin/darwin-rebuild switch --flake ".#$HOST_TARGET"
  else
    fail "Unsupported Operating System: $(uname)"
    # Potentially nix build following by ./result/activate
  fi
}

sync_flake
