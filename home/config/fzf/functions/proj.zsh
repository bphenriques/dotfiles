__proj_root() {
  cd "$WORKSPACE"
}

__proj_from_git_ssh() {
  local target_repo="$1"
  local name
  name="$(basename "$target_repo" .git)"

  if [ ! -d "${WORKSPACE}/${name}" ]; then
    git clone "$target_repo" "$WORKSPACE/$name"
  fi

  __proj_goto "$name"
}

__proj_from_git_url() {
  local url="$1"
  local org name
  org="$(basename $(dirname "$url"))"
  name="$(basename "$url")"

  if [ ! -d "${WORKSPACE}/${name}" ]; then
    git clone "git@github.com:$org/$name.git" "$WORKSPACE/$name"
  fi

  __proj_goto "$name"
}

__proj_goto() {
  local target="$1"
  cd "$WORKSPACE/${target}" 2>/dev/null
}

if [ ! -d "${WORKSPACE}" ]; then
  echo "WORKSPACE is not set or not a directory!"
  exit 1
fi

case "$1" in
  "")                 __proj_root                       ;;
  "git@"*".git")      __proj_from_git_ssh "$1"          ;;
  "https://"*".git")  echo "Not a supported operation"  ;;
  "https://"*".git")  echo "Not a supported operation"  ;;
  "https://"*)        __proj_from_git_url "$1"          ;;
  *)                  __proj_goto "$1"                  ;;
esac
