#shellcheck shell=bash

search="${1-}"
fd 2>/dev/null | fzf --ansi --query "$search" --prompt="Find file: " --preview="preview {}"
