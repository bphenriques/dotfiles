#!/bin/sh
set -e

if [ ! -d "/Applications/Emacs.app" ]; then
    ln -s /usr/local/opt/emacs-plus/Emacs.app /Applications/Emacs.app
fi