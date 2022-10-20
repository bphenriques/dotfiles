#!/bin/sh
# shellcheck disable=SC1091
set -euf
SCRIPT_PATH="$(dirname "$0")"
# shellcheck source=util.sh
. "$SCRIPT_PATH"/util.sh

# Check type of host.
HOST_FILE_LOCATION="$HOME/.dotfiles/.nix-host"
[ ! -f "$HOST_FILE_LOCATION" ] && fail "$HOST_FILE_LOCATION not found"
HOST_TARGET=$(cat "$HOST_FILE_LOCATION")

XDG_CONFIG_HOME=${XDG_CONFIG_HOME:-"$HOME"/.config} # Set if absent.
DOOM_EMACS_PATH="$XDG_CONFIG_HOME"/emacs
WORKSPACE="$HOME/workspace"

sync_flake() {
  info "Syncing Host '$HOST_TARGET'"
  nix build ".#$HOST_TARGET"
  case "$(uname -s)" in
      Darwin)     ./result/sw/bin/darwin-rebuild switch --flake ".#$HOST_TARGET"
                  ;;
      *)          ./result/activate
                  ;;
  esac
}


sync_emacs() {
  if [ ! -d "$DOOM_EMACS_PATH" ]; then
      info 'Doom Emacs - Not installed. Installing...'
      git clone --depth 1 https://github.com/hlissner/doom-emacs "$DOOM_EMACS_PATH"
      "$DOOM_EMACS_PATH"/bin/doom install
      emacs --batch -f all-the-icons-install-fonts
      success 'Doom Emacs - Done!'
  fi

  info 'Doom Emacs - Syncing...'
  "$XDG_CONFIG_HOME"/emacs/bin/doom sync
  success 'Doom Emacs - Done!'
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

sync_repository "$HOME/.dotfiles"
sync_flake
sync_emacs
sync_repository "$WORKSPACE/journal"
sync_repository "$WORKSPACE/knowledge-base"

