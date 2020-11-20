#!/bin/sh
# shellcheck disable=SC1091
set -euf 
SCRIPT_PATH="$(dirname "$0")"
# shellcheck source=util.sh
. "$SCRIPT_PATH"/util.sh

# Set if absent.
XDG_CONFIG_HOME=${XDG_CONFIG_HOME:-"$HOME"/.config}

(command -v nix > /dev/null && success 'Nix - Installed!') || fail 'Nix - Not installed!'
(command -v nix flake check > /dev/null && success 'Nix flake - Installed!') || fail 'Nix flake - Not installed!'
(command -v emacs > /dev/null && success 'Emacs - Installed!') || fail 'Emacs - Not installed!'

case "$(uname -s)" in
    Darwin)     (command -v brew > /dev/null && success 'Homebrew - Installed!') || fail 'Homebrew - Not installed!'
                ;;
   *)           ;;
esac
