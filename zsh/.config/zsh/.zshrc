# History - http://zsh.sourceforge.net/Doc/Release/Options.html @ 16.2.4 History
HISTFILE="$ZDOTDIR/.zsh_history"                                                    # Change default file away from my $HOME.
HISTSIZE=10000                                                                      # Number of entries to keep in memory.
SAVEHIST=$HISTSIZE                                                                  # Number of entries to keep in file.
HISTIGNORE="ls:la:cd:up:mkcd"                                                       # Filter uninteresting commands.
setopt HIST_IGNORE_DUPS HIST_IGNORE_ALL_DUPS HIST_REDUCE_BLANKS HIST_IGNORE_SPACE   # How entries are stored/evicted.
setopt SHARE_HISTORY APPEND_HISTORY INC_APPEND_HISTORY                              # Share between sessions and write immediately.

# Antibody - http://getantibody.github.io/
source <(antibody init)
antibody bundle < "$HOME/.config/zsh/antibody_plugins"

# Theme - https://github.com/romkatv/powerlevel10k
source "$HOME/.config/zsh/powerlevel10k.theme.zsh"

# Functions - Lazy autoload every file in "$ZDOTDIR/functions"
fpath=("$ZDOTDIR/functions" $fpath);
autoload -Uz $fpath[1]/*(.:t)

# Aliases
source "$ZDOTDIR/aliases.zsh"

# Auto compeltions
source "$ZDOTDIR/auto-completions.zsh"

# Fzf
[ -f "${XDG_CONFIG_HOME:-$HOME/.config}"/fzf/fzf.zsh ] && source "${XDG_CONFIG_HOME:-$HOME/.config}"/fzf/fzf.zsh
