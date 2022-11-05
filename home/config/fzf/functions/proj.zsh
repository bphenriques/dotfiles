test ! -d "$WORKSPACE" && echo "Environment variable WORKSPACE is not set or not a valid directory!" && return

project="$1"
shift

if [[ -d "$WORKSPACE/$project" ]]; then
  cd "$WORKSPACE/$project"
else
  cd "$WORKSPACE"
fi
