#compdef proj
local project=$( ls -d "$WORKSPACE"/* | xargs -n 1 basename 2> /dev/null | fzf --layout=reverse )
compadd $project

# Workaround to make fzf work for completion definitions. Based on https://github.com/lincheney/fzf-tab-completion.
TRAPEXIT() {
   zle reset-prompt
}
return 0
