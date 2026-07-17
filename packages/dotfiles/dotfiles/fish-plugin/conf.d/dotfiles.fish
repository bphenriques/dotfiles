if not set -q DOTFILES_CMD
    set -g DOTFILES_CMD dot
end

if not set -q DOTFILES_LOCATION
    set -g DOTFILES_LOCATION "$HOME/.dotfiles"
end

function $DOTFILES_CMD -d dotfiles
    __dotfiles $argv
end
