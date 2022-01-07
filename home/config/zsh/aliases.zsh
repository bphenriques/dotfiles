# Files
alias ls='ls --color=auto'
alias la='ls -la'
alias mkdir='mkdir -pv'

# Text Processing
alias grep='grep --color'

# Test Editor
alias emacs='emacsclient --tty --alternate-editor ""' # Start emacs daemon if not running already.
alias e="$EDITOR"

# Java
alias javals='/usr/libexec/java_home -V'
alias java8='export JAVA_HOME=$(/usr/libexec/java_home -v1.8)'
alias java11='export JAVA_HOME=$(/usr/libexec/java_home -v11)'

# Nix
alias nix-cleanup='nix-collect-garbage -d --delete-older-than 30d'

# Shortcuts
alias dotfiles='cd "$HOME"/.dotfiles'
