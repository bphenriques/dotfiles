#shellcheck shell=bash

#shellcheck disable=SC2016
__fish_shell() {
  echo '
function ffd-widget
  set --local buffer (builtin commandline --current-buffer | string trim -l)

  if test -z $buffer
    set --local target (fzf-fd)

    not test -z $target
    and if test -f $target
      $EDITOR $target
    else if test -d $target
      # Ensure prompt does not get broken if I cancel
      builtin commandline --function cancel-commandline repaint
      cd $target
    end
  else
    # if buffer is not empty, replace the current token with the search result
    set --local current_token (builtin commandline --current-token)
    set --local target (fzf-fd $current_token)
    builtin commandline -rt $target
    and builtin commandline --function repaint
  end
end

function ffd
  set --local target (fzf-fd $argv[1])

  not -z $target
  and if test -f $target
    $EDITOR $target
  else if test -d $target
    # Ensure prompt does not get broken if I cancel
    builtin commandline --function cancel-commandline repaint
    cd $target
  end
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
    search="${1-}"
    fd 2>/dev/null | fzf --print0 --ansi --query "$search" --prompt="Find file: " --preview="preview {}"
    ;;
esac
