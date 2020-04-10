#!/bin/sh
set -e

if [ ! -d "$HOME/.emacs.d" ]; then
    git clone https://github.com/hlissner/doom-emacs "$HOME/.emacs.d"
    "$HOME/.emacs.d/bin/doom" install --yes
fi
