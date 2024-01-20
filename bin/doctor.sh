#!/usr/bin/env sh
# shellcheck disable=SC1091
set -uf
SCRIPT_PATH="$(dirname "$0")"
# shellcheck source=util.sh
. "$SCRIPT_PATH"/util.sh

# Set if absent.
DOTFILES_LOCATION="$HOME/.dotfiles"
HOST_FILE_LOCATION="$DOTFILES_LOCATION"/.nix-host

check_shell() {
  case $SHELL in
    */fish) success 'Shell - Already set correctly!' ;;
    *)
      location="$(which fish 2>/dev/null)"
      if [ $? -eq 0 ]; then
        warn "Fish is not the current shell! You may set using: chsh -s '$location'"
      else
        warn "Fish is not available in PATH"
      fi
      ;;
  esac
}

(command -v nix >/dev/null && success 'Nix - Installed!') || fail 'Nix - Not installed!'
(command -v nix flake show templates >/dev/null && success 'Nix flake - Installed!') || fail 'Nix flake - Not installed!'
case "$(uname -s)" in
  Darwin)
    (command -v brew >/dev/null && success 'Homebrew - Installed!') || fail 'Homebrew - Not installed!'
    ;;
  *) ;;
esac

(test -d "$DOTFILES_LOCATION/host/$(cat "$HOST_FILE_LOCATION")" && success "Nix Host - Set to '$(cat "$HOST_FILE_LOCATION")'!") || fail "Nix Host - Invalid host! It is $(cat "$HOST_FILE_LOCATION")"

check_shell
