#compdef proj
project="$(find "$WORKSPACE" -type d -mindepth 1 -maxdepth 1 -print0 | xargs -0 basename | sort | fzf --layout=reverse)"
compadd "$project"

# Workaround to make fzf work for completion definitions. Based on https://github.com/lincheney/fzf-tab-completion.
TRAPEXIT() {
  zle accept-line
  zle reset-prompt
}
return 0
