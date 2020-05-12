#!/bin/sh
set -e
SCRIPT_PATH="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
# shellcheck source=helpers.sh
source "$SCRIPT_PATH/helpers.sh"

DEFAULT_STEPS_ORDER=( install_packages stow_dotfiles )

stow_dotfiles() {
    module="$1"
    location="$2"

    info "$module - Stowing.."
    stow --dir "$SCRIPT_PATH" --target "$HOME" "$module"
}

run() {
    custom_script="$1"
    module="$2"
    location="$3"

    info "$module - Running $custom_script"
    sh "$location/$custom_script"
}

install_module() {
    module="$1"
    location="$2"
    steps_override="$3"

    if [ ! -d "$location" ]; then
        fail "$module - Does not exist!"            
    fi

    installation_steps="${DEFAULT_STEPS_ORDER[@]}"
    if [ -f "$steps_override" ]; then
        installation_steps=()
        while IFS='\n' read -r step; do
            case ${step%% *} in
                install_packages)   installation_steps+=("$step")
                                    ;;
                stow_dotfiles)      installation_steps+=("$step") 
                                    ;;
                run)                installation_steps+=("$step") 
                                    ;;
                *)                  fail "Illegal step $step"
                                    ;;
            esac            
        done < "$steps_override"
    fi

    for step in "${installation_steps[@]}"; do
        $step "$1" "$2"
    done
}

case $( uname -s ) in
    Darwin) source installer.macos.sh
            steps_override_file='installer.macos.steps'
            ;;
    *)      fail "Unsupported Operating System"
            ;;
esac

info 'Installing Pre-Requirements'
install_prequirements
success 'Pre-Requirements'

modules=( "$@" )
for module in "${modules[@]}"; do
    info "$module - Installing.."
    install_module "$module" "$SCRIPT_PATH/$module" "$SCRIPT_PATH/$module/$steps_override_file"
    success "$module"
done

success 'Done'