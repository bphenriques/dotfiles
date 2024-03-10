#!/usr/bin/env sh
# shellcheck disable=SC1091
set -euf
SCRIPT_PATH="$(dirname "$0")"
# shellcheck source=util.sh
. "$SCRIPT_PATH"/util.sh

DEBUG=1 #${DEBUG:-0}
DISABLE_CACHE=1 # ${DISABLE_CACHE:-0}

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

sync_repository() {
  location="$1"
  name="$(basename "$location")"

  if [ ! -d "$location" ]; then
    fail "Repo '$name' - Does not exist!"
  fi

  # If it is out-of-sync and behind, rebase which should fail automatically if is dirty which is expected.
  if [ "$(git -C "$location" rev-parse HEAD)" = "$(git -C "$location" ls-remote $(git -C "$location" rev-parse --abbrev-ref @{u} | sed 's/\// /g') | cut -f1)" ]; then
    info "Repo '$name' - Up to date! Nothing to do!"
  else
    git -C "$location" fetch origin
    if (git status -uno | grep --quiet "branch is ahead"); then
      warn "Repo '$name' - Has unpushed changes!"
    else
      info "Repo '$name' - Pulling changes..."
      git -C "$location" pull --rebase
    fi
  fi
  success "Repo '$name' - Done!"
}

sync_flake
