initial_query="${1:-}"
search='rg --column --line-number --no-heading --color=always --smart-case '

# shellcheck disable=SC2034
FZF_DEFAULT_COMMAND="$search '$initial_query' ."

fzf --bind "change:reload:sleep 0.05;$search {q} . || true" \
  --ansi --query "$initial_query" --disabled \
  --delimiter : \
  --preview "rg --ignore-case --pretty --context 10 '{q}' {1}" |
  cut -d ':' -f1
