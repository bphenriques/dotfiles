#compdef proj
project="$(find "$WORKSPACE" -mindepth 1 -maxdepth 1 -type d -print0 | xargs -I {} -0 basename {} | sort | fzf --layout=reverse)"
compadd "$project"

# Workaround to make fzf work for completion definitions. Based on https://github.com/lincheney/fzf-tab-completion.
TRAPEXIT() {
  zle accept-line
  zle reset-prompt
}
return 0
