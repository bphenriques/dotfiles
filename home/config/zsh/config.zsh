#
# History - http://zsh.sourceforge.net/Doc/Release/Options.html @ 16.2.4 History
#
HISTFILE="$ZDOTDIR"/.zsh_history # Change default file away from zsh folder.
HISTSIZE=10000                   # Number of entries to keep in memory.
SAVEHIST="$HISTSIZE"             # Number of entries to keep in file.

setopt APPEND_HISTORY            # Appends history to history file on exit
setopt INC_APPEND_HISTORY        # Write to the history file immediately, not when the shell exits.
setopt SHARE_HISTORY             # Share history between all sessions.
setopt HIST_EXPIRE_DUPS_FIRST    # Expire a duplicate event first when trimming history.
setopt HIST_IGNORE_DUPS          # Do not record an event that was just recorded again.
setopt HIST_IGNORE_ALL_DUPS      # Delete an old recorded event if a new event is a duplicate.
setopt HIST_FIND_NO_DUPS         # Do not display a previously found event.
setopt HIST_IGNORE_SPACE         # Do not record an event starting with a space.
setopt HIST_SAVE_NO_DUPS         # Do not write a duplicate event to the history file.
setopt HIST_VERIFY               # Do not execute immediately upon history expansion.
setopt HIST_BEEP                 # Beep when accessing non-existent history.

#
# Directories
#
setopt AUTO_CD                   # Auto changes to a directory without typing cd.

#
# More interactive sessions settings
#
export GPG_TTY=$(tty)                           # Load GPG.
export CLICOLOR=1                               # Enable ls colors in MacOS. Is it relevant when using coreutils?
export LS_COLORS="$(vivid generate snazzy)"     # Generates the color palette. Alternative is to use `eval "$(dircolors <path>)"" with an awkard syntax.