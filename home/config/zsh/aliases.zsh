# Files
alias ls='ls --color=auto'
alias la='ls -la'
alias mkdir='mkdir -pv'

# Text Processing
alias grep='grep --color'

# Test Editor
alias emacs='emacsclient --tty --alternate-editor ""' # Start emacs daemon if not running already.
alias e="$EDITOR"
alias killemacs='emacsclient --eval "(kill-emacs)"'

# Java
alias javals='/usr/libexec/java_home -V'

# Nix
alias nix-cleanup='nix-collect-garbage -d --delete-older-than 30d'

# Shortcuts
alias dotfiles='cd "$HOME"/.dotfiles'

# Tmux
alias reload='tmux respawn-pane -k'
