# Override zsh folders to unclutter $HOME folder
export XDG_CONFIG_HOME="$HOME/.config"
export ZDOTDIR="$XDG_CONFIG_HOME/zsh"

# Set locale and UTF-8
export LANG=en_US.UTF-8
export LC_CTYPE=en_US.UTF-8

# Default editors
export EDITOR=vim
export VISUAL=vim
export PAGER="less -iMR"

# Enable ls colors in MacOS (TODO: Consistent coloring between different OSs
export CLICOLOR=1

# Configure GPG
export GPG_TTY=$(tty)

# Sensitive/local variables are stored separately
export PRIVATE_VARS="$HOME/.zshenv.local"
[ -s "$PRIVATE_VARS" ] && source "$PRIVATE_VARS"
