#!/bin/sh
# shellcheck disable=SC1091
set -euf 
SCRIPT_PATH="$(dirname "$0")"
# shellcheck source=util.sh
. "$SCRIPT_PATH"/util.sh

XDG_CONFIG_HOME=${XDG_CONFIG_HOME:-"$HOME"/.config} # Set if absent.

info 'Nix Flake - Upgrading...'
nix flake update
success 'Nix Flake - Complete!'

info 'Doom Emacs - Upgrading...'
"$XDG_CONFIG_HOME"/emacs/bin/doom upgrade
success 'Doom Emacs - Complete!'

case "$(uname -s)" in
    Darwin)     info 'Homebrew - Upgrading...'
                brew upgrade && brew update
                success 'Homebrew - Complete!'
                ;;
   *)           ;;
esac
