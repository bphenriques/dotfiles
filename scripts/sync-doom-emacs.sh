#!/bin/sh
# shellcheck disable=SC1091
set -euf 
SCRIPT_PATH="$(dirname "$0")"
# shellcheck source=util.sh
. "$SCRIPT_PATH"/util.sh

XDG_CONFIG_HOME=${XDG_CONFIG_HOME:-"$HOME"/.config} # Set if absent.
DOOM_EMACS_PATH="$XDG_CONFIG_HOME"/emacs

# Must be installed after wards
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
