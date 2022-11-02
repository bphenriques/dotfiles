__FZF_CUSTOM_COMPRUN_PREVIEW_DIRECTORY="([ -d {-1} ] && tree -C {-1} | head -n 50)"
__FZF_CUSTOM_COMPRUN_PREVIEW_FILE="([ -f {-1} ] && bat --style=numbers --color=always {-1} 2>/dev/null)"

local command=$1
shift

case "$command" in
  # By default, show a preview if it is a folder or file.
  *)                fzf "$@" --preview "$__FZF_CUSTOM_COMPRUN_PREVIEW_DIRECTORY || $__FZF_CUSTOM_COMPRUN_PREVIEW_FILE" ;;
esac
