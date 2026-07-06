#shellcheck shell=bash

initial_query="${1:-}"
rg_prefix='rg --column --line-number --no-heading --color=always --smart-case'

fzf --ansi --disabled --query "$initial_query" \
  --bind "start:reload:$rg_prefix -- {q} ." \
  --bind "change:reload:sleep 0.05; $rg_prefix -- {q} . || true" \
  --prompt "Find text: " \
  --delimiter : \
  --preview 'bat --color=always --style=numbers --highlight-line {2} -- {1}' \
  --preview-window '+{2}/3' \
  --accept-nth '{1}:{2}'
