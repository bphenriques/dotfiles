__proj_clone_repo() {
  target="$(basename "$1" .git)"
  if [ ! -d "$PROJ_ROOT/$target" ]; then
    git clone "$target" "$PROJ_ROOT/$target"
  fi
  printf %s "$PROJ_ROOT/$target"
}

__proj_find() {
  target="$1"
  if [ -d "$PROJ_ROOT/$target" ]; then
    printf %s "$PROJ_ROOT/$target"
  else
    __proj_select "${target}"
  fi
}

__proj_select() {
  target="${1-}"
  # shellcheck disable=SC2016
  fd --base-directory "$PROJ_ROOT" --type directory --max-depth 1 --exec basename \
    | fzf --exit-0 --select-1 --no-multi --query="$target" --layout=reverse --preview='preview "$PROJ_ROOT"/{}' \
    | xargs -I{} printf %s/%s "$PROJ_ROOT" {}
}

if [ ! -d "${PROJ_ROOT}" ]; then
  printf "Error! PROJ_ROOT is not set or not a directory!"
  exit 1
fi

search="${1-}"
result=
case "$search" in
  "")                 result="${PROJ_ROOT}"                       ;;
  "--select")         result="$(__proj_select)"                   ;;
  *"github.com"*)     result="$(__proj_clone_repo "$search")"     ;;
  *)                  result="$(__proj_find "$search")"           ;;
esac

if [ -d "${result}" ]; then
  printf %s "${result}"
else
  printf "Error: no match found for %s" "${search}"
  exit 2
fi
