search="${1-}"
fd 2>/dev/null | fzf --ansi --query "$search" --prompt="> " --preview="preview {}"
