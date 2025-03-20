#shellcheck shell=bash

# Assumption: the library is organized using the standard (?) Artist/Album/Song. Works for me. Might not work with you.
select_title_artist_album() {
  mpc -f '%file%\t%title%\t%artist%\t%album%' listall | while IFS=$'\t' read -r file title artist album; do
    printf "%s\t%s\u0000icon\u001f%s\n" "$artist" "$artist" "$MPC_UTIL_ARTIST_ICON"
    printf "%s\t%s by %s\u0000icon\u001f%s\n" "$artist/$album" "$album" "$artist" "$MPC_UTIL_ALBUM_ICON"
    printf "%s\t%s by %s (%s)\u0000icon\u001f%s\n" "$file" "$title" "$artist" "$album" "$MPC_UTIL_SONG_ICON"
  done | LC_ALL=C sort --unique | fuzzel --dmenu --with-nth=2 --accept-nth=1
}

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
  dmenu-any)
    shift 1
    selection="$(select_title_artist_album)"
    [ "$selection" != "" ] && mpc_file "${1:-play}" "$selection"
    ;;
esac