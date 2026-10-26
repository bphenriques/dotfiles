# fzf-rg emits file:line
frg() {
  local target
  target="$(fzf-rg "$1")" || return
  [[ -f ${target%%:*} ]] && "$EDITOR" "$target"
}

# Empty buffer: open like `frg`. Otherwise replace the current word with the file, dropping :line.
__frg-widget() {
  local target token
  if [[ -z ${BUFFER// } ]]; then
    target="$(fzf-rg)"
    if [[ -f ${target%%:*} ]]; then
      BUFFER="$EDITOR ${(q)target}"
      zle accept-line
    fi
  else
    token="${LBUFFER##* }"
    target="$(fzf-rg "$token")" && LBUFFER="${LBUFFER%"$token"}${(q)target%%:*}"
  fi
  zle reset-prompt
}
zle -N __frg-widget
