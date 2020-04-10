#!/bin/sh
set -e

# Setup fzf for zsh
"$(brew --prefix)/opt/fzf/install" --xdg --key-bindings --completion --update-rc --no-bash --no-fish