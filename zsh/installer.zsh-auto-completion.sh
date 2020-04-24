#!/bin/sh
set -e

# Setup fzf for zsh. Fortunately, it is idempotent (checks if the line exists in the configuration file)
"$(brew --prefix)/opt/fzf/install" --xdg --key-bindings --completion --update-rc --no-bash --no-fish