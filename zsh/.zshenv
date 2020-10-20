# Override zsh folders to unclutter $HOME folder
export XDG_CONFIG_HOME="$HOME/.config"
export ZDOTDIR="$XDG_CONFIG_HOME/zsh"

# Set locale and UTF-8
export LANG=en_US.UTF-8
export LC_ALL=en_US.UTF-8

# Enable ls colors in MacOS
export CLICOLOR=1

# Default editors
export EDITOR=vim
export VISUAL=vim
export PAGER="less -iMR"

# Configure GPG
export GPG_TTY=$(tty)

# Default directory for repositories
export REPOS=$HOME/Documents/repos

# Sensitive/local variables are stored separately
export PRIVATE_VARS="$HOME/.zshenv.local"
[ -s "$PRIVATE_VARS" ] && source "$PRIVATE_VARS"
