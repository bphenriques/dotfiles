# shellcheck shell=sh

PROJ_ROOT="${PROJ_ROOT:-XDG_DOCUMENTS_DIR}"

__proj_clone_repo() {
  target="$(basename "$1" .git)"
  if [ ! -d "$PROJ_ROOT/$target" ]; then
    git clone "$target" "$PROJ_ROOT/$target"
  fi
  printf %s "$PROJ_ROOT/$target"
}

__proj_root() {
  printf %s "${PROJ_ROOT}"
}

__proj_select() {
  target="${1-}"
  # shellcheck disable=SC2016
  fd --base-directory "$PROJ_ROOT" --type directory --max-depth 1 --exec basename \
      | fzf --prompt "Switch to project: " --exit-0 --select-1 --no-multi --query="$target" --layout=reverse --preview='preview "$PROJ_ROOT"/{}' \
      | while read -r project; do printf %s/%s "$PROJ_ROOT" "$project"; done
}

# shellcheck disable=SC2016
__proj_fish_shell() {
  echo '
function p-widget
  set --local buffer (commandline --current-buffer)
  set --local target (project --select $buffer)

  # Ensure prompt does not get broken if I cancel
  builtin commandline --function cancel-commandline repaint

  not test -z $target
  and test -d $target
  and cd $target
end

function p
  if test (count $argv) -eq 0
    cd $PROJ_ROOT
  else
    set --local target (project --select $argv[1])

    # Ensure prompt does not get broken if I cancel
    builtin commandline --function cancel-commandline repaint

    not test -z $target
    and test -d $target
    and cd $target
  end
end
'
}

if [ ! -d "${PROJ_ROOT}" ]; then
  printf "Error! PROJ_ROOT is not set or not a directory!"
  exit 1
fi

case "${1:-}" in
  --root)   __proj_root ;;
  --select)
    shift 1

    search="${1-}"
    result=
    case "$search" in
      *"github.com"*) result="$(__proj_clone_repo "$search")" ;;
      *)              result="$(__proj_select "$search")"     ;;
    esac

    if [ -d "${result}" ]; then
      printf %s "${result}"
    else
      printf "Error: no match found for %s" "${search}"
      exit 2
    fi
    ;;
  --init-shell)
    shift 1
    case "${1:-}" in
      fish) __proj_fish_shell                     ;;
      *)    echo "Unsupported shell: $1"; exit 1  ;;
    esac
    ;;
esac
