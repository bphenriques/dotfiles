#shellcheck shell=bash

search="${1-}"
fd 2>/dev/null | fzf --print0 --ansi --query "$search" --prompt="Find file: " --preview="preview {}"
