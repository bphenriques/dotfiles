#shellcheck shell=bash

XDG_CACHE_HOME="${XDG_CACHE_HOME:-"$HOME"/.cache}"
MPC_UTIL_CACHE_DIR="${MPC_UTIL_CACHE_DIR:-"${XDG_CACHE_HOME}"/mpc-util}"
mkdir -p "${MPC_UTIL_CACHE_DIR}"

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

mpc_artwork_icon() {
  art_cache_key="$(mpc -f '%artist%_%album%_%title%' | head -1)"
  if [ ! -f "$MPC_UTIL_CACHE_DIR/$art_cache_key.png" ]; then
    # TODO: consider resizing or set a fallback for files that dont have it
    mpc readpicture "$(mpc -f '%file%' | head -1)" > "$MPC_UTIL_CACHE_DIR/$art_cache_key.png"
  fi
  echo -n "$MPC_UTIL_CACHE_DIR/$art_cache_key.png"
}

notify_current_status() {
  title=
  icon=
  description=
  case "$(mpc status '%state%')" in
    paused)
      title="Paused"
      icon="$(mpc_artwork_icon)"
      description="$(mpc -f '%title% by %artist%' | head -1)"
      ;;
    stopped)
      title="Nothing playing"
      icon=""
      description="${1:-}"
      ;;
    playing)
      title="Now playing"
      icon="$(mpc_artwork_icon)"
      description="$(mpc -f '%title% by %artist%' | head -1)"
      ;;
  esac

  progress="$(mpc volume | grep -Po '[\d]+')"
  notify-send \
    --expire-time 5000 \
    --icon "$icon" \
    --category "mpc-current" \
    --hint string:x-canonical-private-synchronous:mpd-current \
    --hint string:x-dunst-stack-tag:mpd-current \
    --hint int:value:"$progress" \
    --transient \
    "${title}" "${description}"
}

case "${1:-}" in
  play-pause)       mpc toggle && notify_current_status                     ;;
  previous)         mpc prev && notify_current_status                       ;;
  next)             mpc next && notify_current_status                       ;;
  clear)            mpc clear && notify_current_status "Music queue clear"  ;;
  stop)             mpc stop && notify_current_status "Stopped"             ;;
  volume-increase)  mpc volume "+${2:-5}" && notify_current_status          ;;
  volume-decrease)  mpc volume "-${2:-5}" && notify_current_status          ;;
  play-shuffled)
    mpc clear
    mpc add /
    mpc play
    notify_current_status
    ;;
  dmenu-any)
    shift 1
    selection="$(select_title_artist_album)"
    if [ "$selection" != "" ]; then
      mpc_file "${1:-play}" "$selection"
      notify_current_status
    fi
    ;;
esac