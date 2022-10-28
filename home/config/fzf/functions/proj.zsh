test -z "$WORKSPACE" && echo "Environment variable WORKSPACE is not set. Aborting!" && exit 1

__proj_usage() {
   echo "proj [name]"
}

__proj_cd() {
  cd "$WORKSPACE"
}

__proj_find_project() {
  project_name="$1"
  ls -d "$WORKSPACE"/* | xargs -n 1 basename | \
    fzf --filter="$project_name" | \
    fzf --exit-0 --select-1 --layout=reverse
}

__proj_goto() {
  project_name="$1"
  target="$(__proj_find_project "$1")"
  if [[ -d "$WORKSPACE/$target" ]]; then
    cd "$WORKSPACE/$target"
  else
    __proj_cd
  fi
}

if [ $# -eq 0 ]; then
  __proj_cd
elif [ $# -eq 1 ]; then
  __proj_goto "$1"
else
  __proj_usage
fi
