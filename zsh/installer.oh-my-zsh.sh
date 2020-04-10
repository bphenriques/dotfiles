#!/bin/sh
set -e

# TODO more reliable way. Or the installation script could be idempotent... :shrug:
if [ ! -d "$HOME/.oh-my-zsh" ]; then
    sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"
    chsh -s "$(which zsh)"    
fi