#shellcheck shell=bash

# printf '%s by %s (%s)\u0000icon\u001f%s\n' "$(echo "$line" | awk '{$1=$1};1')" "$current_artist" "$current_album" "/home/bphenriques/image.png"        ;;

select_title_file() { mpc -f '%file%\t%title% by %artist% (%album%)' listall | fuzzel -d --with-nth=2 --accept-nth=1; }
select_artist() { mpc ls | fuzzel -d; }

mpc_file() {
  case "$1" in
    play)     mpc clear && mpc add "$2" && mpc play ;;
    next)     mpc insert "$2" && mpc play           ;;
    add)      mpc add "$2" && mpc play              ;;
  esac
}

case "${1:-}" in
  play-pause)     mpc toggle      ;;
  previous)       mpc prev        ;;
  next)           mpc next        ;;
  clear)          mpc clear       ;;
  stop)           mpc stop        ;;
  play-shuffled)
    mpc clear
    mpc add /
    mpc play
    ;;
  dmenu-title)
    shift 1
    selection="$(select_title_file)"
    [ "$selection" != "" ] && mpc_file "${1:-play}" "$selection"
    ;;
  dmenu-artist)
    shift 1
    selection="$(select_artist)"
    [ "$selection" != "" ] && mpc_file "${1:-play}" "$selection"
    ;;
esac