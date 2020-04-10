#!/bin/sh
set -e
SCRIPT_PATH="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
# shellcheck source=helpers.sh
source "$SCRIPT_PATH/helpers.sh"

install_prequirements() {
    if ! command_exists brew; then
        info "Installing homebrew..."
        xcode-select --install
        ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
    fi

    if ! command_exists stow; then
        info 'Installing Stow'
        HOMEBREW_NO_AUTO_UPDATE=1 brew install stow
    fi
}

install_packages() {
    module="$1"
    location="$2"
    brewfile="$location/Brewfile"
    
    if [ -f "$brewfile" ]; then
        info "$module - brew bundle $brewfile"
        HOMEBREW_NO_AUTO_UPDATE=1 brew bundle --no-lock --file "$brewfile"
    fi
}
