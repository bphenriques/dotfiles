# History - http://zsh.sourceforge.net/Doc/Release/Options.html @ 16.2.4 History
HISTFILE="$ZDOTDIR/.zsh_history"                                                    				# Change default file away from my $HOME.
HISTSIZE=10000                                                                      				# Number of entries to keep in memory.
SAVEHIST=$HISTSIZE                                                                  				# Number of entries to keep in file.
HISTORY_IGNORE="(ls *|la *|cd *|mkcd *|man *|rm *|git add *|mkdir *)"                                           # Filter uninteresting commands.
setopt HIST_IGNORE_SPACE HIST_IGNORE_DUPS HIST_IGNORE_ALL_DUPS HIST_REDUCE_BLANKS HIST_IGNORE_SPACE   		# How entries are stored/evicted.
setopt SHARE_HISTORY APPEND_HISTORY INC_APPEND_HISTORY                             		 		# Share between sessions and write immediately.

# http://zsh.sourceforge.net/Doc/Release/Parameters.html#Parameters-Used-By-The-Shell
zshaddhistory() {
  emulate -L zsh
  ## uncomment if HISTORY_IGNORE
  ## should use EXTENDED_GLOB syntax
  # setopt extendedglob
  [[ $1 != ${~HISTORY_IGNORE} ]]
}

# EDITOR=vi affects keybindings. Revert back to emacs for some sanity (for now).
bindkey -e

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

# Auto completions
source "$ZDOTDIR/auto-completions.zsh"

# Fzf
[ -f "${XDG_CONFIG_HOME:-$HOME/.config}"/fzf/fzf.zsh ] && source "${XDG_CONFIG_HOME:-$HOME/.config}"/fzf/fzf.zsh
export FZF_DEFAULT_OPTS="--bind='ctrl-p:toggle-preview --bind='ctrl-a:select-all' --bind='ctrl-f:jump' --marker='* ' --pointer='▶'"
export FZF_DEFAULT_COMMAND="rg --smart-case --files --no-ignore --hidden --follow --glob '!{.git,node_modules}/*'"
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"

# Create tmux sessions per new window
[ -z "$TMUX" ] && { exec tmux new-session && exit; }

# Custom Tmux initializer. Find and open project given the window name.
[[ -v TMUX_NEW_WINDOW_INIT ]] && proj $(tmux display-message -p '#{window_name}') && tmux rename-window -t $(tmux display-message -p '#I') "$(basename $PWD)" 

