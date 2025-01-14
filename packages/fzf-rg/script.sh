#shellcheck shell=bash

#shellcheck disable=SC2016
__fish_shell() {
  echo '
function frg-widget
  set --local buffer (builtin commandline --current-buffer | string trim -l)

  if test -z $buffer
    set --local target (fzf-rg)
    not test -z $target
    and test -f $target
    and $EDITOR $target
  else
    # if buffer is not empty, replace the current token with the search result
    set --local current_token (builtin commandline --current-token)
    set --local target (fzf-rg $current_token)
    builtin commandline -rt $target
    and builtin commandline --function repaint
  end
end

function frg
  set --local target (fzf-rg $argv[1])
  not test -z $target
  and test -f $target
  and $EDITOR $target
end
'
}

case "${1:-}" in
  --init-shell)
    shift 1
    case "${1:-}" in
      fish) __fish_shell                     ;;
      *)    echo "Unsupported shell: $1"; exit 1  ;;
    esac
    ;;
  *)
    initial_query="${1:-}"
    search='rg --column --line-number --no-heading --color=always --smart-case '

    # shellcheck disable=SC2034
    FZF_DEFAULT_COMMAND="$search '$initial_query' ."

    fzf --bind "change:reload:sleep 0.05;$search {q} . || true" \
      --prompt "Find text: " \
      --ansi --query "$initial_query" --disabled \
      --delimiter : \
      --preview "rg --ignore-case --pretty --context 10 '{q}' {1}" |
      cut -d ':' -f1
    ;;
esac
