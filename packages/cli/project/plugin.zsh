p() {
  if (( $# == 0 )); then
    cd "$PROJ_ROOT"
    return
  fi
  local target
  target="$(project --select "$1")" && [[ -d $target ]] && cd "$target"
}

__project-widget() {
  local target
  target="$(project --select "$BUFFER")"
  if [[ -d $target ]]; then
    BUFFER="cd ${(q)target}"
    zle accept-line
  fi
  zle reset-prompt
}
zle -N __project-widget
