# shellcheck shell=sh

PROJ_ROOT="${PROJ_ROOT:-XDG_DOCUMENTS_DIR}"

__proj_clone_repo() {
  target="$(basename "$1" .git)"
  if [ ! -d "$PROJ_ROOT/$target" ]; then
    git clone "$target" "$PROJ_ROOT/$target"
  fi
  printf %s "$PROJ_ROOT/$target"
}

__proj_select() {
  target="${1-}"
  # shellcheck disable=SC2016
  fd --base-directory "$PROJ_ROOT" --type directory --max-depth 1 --exec basename \
    | fzf --prompt "Switch to project: " --exit-0 --select-1 --no-multi --query="$target" --layout=reverse --preview='preview "$PROJ_ROOT"/{}' \
    | xargs -I{} printf %s/%s "$PROJ_ROOT" {}
}

# shellcheck disable=SC2016
__proj_fish_shell() {
  echo 'function proj
  set -l target (command project $argv)
  and cd $target
end'
}

if [ ! -d "${PROJ_ROOT}" ]; then
  printf "Error! PROJ_ROOT is not set or not a directory!"
  exit 1
fi

case "${1:-}" in
  "--init-shell")
    shift 1
    case "${1:-}" in
      fish) __proj_fish_shell                     ;;
      *)    echo "Unsupported shell: $1"; exit 1  ;;
    esac
    ;;
  "") printf %s "${PROJ_ROOT}" ;;
  *)
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
esac
