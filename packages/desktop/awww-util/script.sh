#shellcheck shell=bash

# Contract with the desktop shell: the state file holds the wallpaper last set.
STATE_FILE="${XDG_RUNTIME_DIR:?}/wallpaper"

set_wallpaper() {
  test -f "$1" || return 0
  awww img --transition-type none "$1"
  printf '%s\n' "$1" >"$STATE_FILE" # in place: replacing the file would drop the desktop shell's watch
}

current_file() { cat "$STATE_FILE" 2>/dev/null || true; }

# Never the current one, so every press changes it.
random_file() { find "$1" -type f | grep -vxF -- "$(current_file)" | shuf -n 1; }

# The file after the current one (before it when $2 is -1), wrapping around.
step_file() {
  local current count index=-1
  local -a files
  current="$(current_file)"
  mapfile -t files < <(find "$1" -type f | sort)
  count=${#files[@]}
  [ "$count" -gt 0 ] || return 0
  for i in "${!files[@]}"; do
    [ "${files[$i]}" = "$current" ] && index=$i
  done
  [ "$index" -ge 0 ] || index=$((${2} > 0 ? -1 : 0))
  printf '%s' "${files[$(((index + $2 + count) % count))]}"
}

case "${1:-}" in
  random) set_wallpaper "$(random_file "$2")" ;;
  one) set_wallpaper "$2" ;;
  next) set_wallpaper "$(step_file "$2" 1)" ;;
  prev) set_wallpaper "$(step_file "$2" -1)" ;;
esac
