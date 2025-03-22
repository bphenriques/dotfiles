#shellcheck shell=bash

XDG_CACHE_HOME="${XDG_CACHE_HOME:-"$HOME"/.cache}"
MPC_PLUS_CACHE_DIR="${MPC_PLUS_CACHE_DIR:-"${XDG_CACHE_HOME}"/mpc-plus}"
mkdir -p "${MPC_PLUS_CACHE_DIR}"

select_title_artist_album() {
  mpc -f '%file%\t%title%\t%artist%\t%album%' listall | while IFS=$'\t' read -r file title artist album; do
    # Minor safeguard and assumption: the library is organized using the standard (?) Artist/Album/Song. Works for me. Might not work with you.
    if [ -n "$title" ] && [ -n "$artist" ] && [ -n "$album" ]; then
      printf "%s\t%s\u0000icon\u001f%s\n" "$artist" "$artist" "$MPC_PLUS_ARTIST_ICON"
      printf "%s\t%s by %s\u0000icon\u001f%s\n" "$artist/$album" "$album" "$artist" "$MPC_PLUS_ALBUM_ICON"
      printf "%s\t%s by %s (%s)\u0000icon\u001f%s\n" "$file" "$title" "$artist" "$album" "$MPC_PLUS_SONG_ICON"
    else
      printf "%s\t%s\u0000icon\u001f%s\n" "$file" "$file" "$MPC_PLUS_SONG_ICON"
    fi
  done | LC_ALL=C sort --unique | fuzzel --dmenu --with-nth=2 --accept-nth=1
}

mpc_file() {
  case "$1" in
    play)     mpc clear && mpc add "$2" && mpc play ;;
    next)     mpc insert "$2" && mpc play           ;;
    add)      mpc add "$2" && mpc play              ;;
  esac
}

save_or_compress() {
  if command -v magick >/dev/null 2>&1; then
    magick - -geometry x128 "$destination" >/dev/null 2>&1;
  else
    tee "$destination"
  fi
}

get_mpc_artwork_icon() {
  art_cache_key="$(mpc -f '%artist%_%album%_%title%' | head -1)"
  destination="$MPC_PLUS_CACHE_DIR/$art_cache_key.png"

  # First generate the base image with a reasonable for notifications
  if [ -f "$destination" ]; then
    echo -n "$destination"
  else
    if mpc readpicture "$(mpc -f '%file%' | head -1)" | save_or_compress; then
      echo -n "$destination"
    elif mpc albumart "$(mpc -f '%file%' | head -1)" | save_or_compress; then
      echo -n "$destination"
    else
      echo -n "$MPC_PLUS_SONG_ICON"
    fi
  fi
}

notification_icon() {
  case "$(mpc status '%state%')" in
    playing)  get_mpc_artwork_icon                    ;;
    paused)   get_mpc_artwork_icon                    ;;
    stopped)  echo -n "${MPC_PLUS_STOPPED_ICON}"      ;;
  esac
}

toggle_random() {
  case "$(mpc status '%random%')" in
    off)  mpc random on   ;;
    on)   mpc random off  ;;
  esac
}

toggle_repeat() {
  case "$(mpc status '%repeat%')" in
    off)  mpc repeat on   ;;
    on)   mpc repeat off  ;;
  esac
}

random_status_icon() {
  case "$(mpc status '%random%')" in
    on)   echo -n "${MPC_PLUS_RANDOM_ICON}"      ;;
    off)  echo -n "${MPC_PLUS_NO_RANDOM_ICON}"   ;;
  esac
}

repeat_status_icon() {
  case "$(mpc status '%repeat%')" in
    on)   echo -n "${MPC_PLUS_REPEAT_ICON}"      ;;
    off)  echo -n "${MPC_PLUS_NO_REPEAT_ICON}"   ;;
  esac
}

notify_current_status() {
  title=
  description=
  case "$(mpc status '%state%')" in
    playing)
      title="Now playing"
      description="$(mpc -f '%title%[ by %artist%]' | head -1)"
      ;;
    paused)
      title="Paused"
      description="$(mpc -f '%title%[ by %artist%]' | head -1)"
      ;;
    stopped)
      title="Nothing playing"
      description=""
      ;;
  esac

  notify-send \
    --expire-time 5000 \
    --icon "$(notification_icon)" \
    --category "mpc-current" \
    --hint string:x-canonical-private-synchronous:mpd-current \
    --hint string:x-dunst-stack-tag:mpd-current \
    --transient \
    "${title}" "${description}"
}

notify_action() {
  title="$1"
  description="$2"
  icon="$3"
  notify-send \
    --expire-time 5000 \
    --icon "$icon" \
    --category "mpc-current" \
    --hint string:x-canonical-private-synchronous:mpd-current \
    --hint string:x-dunst-stack-tag:mpd-current \
    --transient \
    "${title}" "${description}"
}

notify_mpd_volume() {
  title=
  description=
  case "$(mpc status '%state%')" in
    playing)
      title="Now playing"
      description="$(mpc -f '%title%[ by %artist%]' | head -1)"
      ;;
    paused)
      title="Paused"
      description="$(mpc -f '%title%[ by %artist%]' | head -1)"
      ;;
    stopped)
      title="Nothing playing"
      description=""
      ;;
  esac

  notify-send \
    --expire-time 5000 \
    --icon "$(notification_icon)" \
    --category "mpc-current" \
    --hint string:x-canonical-private-synchronous:mpd-current \
    --hint string:x-dunst-stack-tag:mpd-current \
    --hint int:value:"$(mpc volume | grep -Po '[\d]+')" \
    --transient \
    "${title}" "${description}"
}

case "${1:-}" in
  play-pause)       mpc toggle                                          ;;
  stop)             mpc stop                                            ;;
  previous)         mpc prev                                            ;;
  next)             mpc next                                            ;;
  volume-increase)  mpc volume "+${2:-5}"                               ;;
  volume-decrease)  mpc volume "-${2:-5}"                               ;;
  play-shuffled)    mpc random on && mpc clear && mpc add / && mpc play ;;
  clear)            mpc clear && notify_action "Music Queue" "No songs" "${MPC_PLUS_CLEAR_ICON}"                        ;;
  toggle-random)    toggle_random && notify_action "Shuffle Songs: $(mpc status '%random%')" "" "$(random_status_icon)" ;;
  toggle-repeat)    toggle_repeat && notify_action "Repeat Song: $(mpc status '%repeat%')" "" "$(repeat_status_icon)"   ;;
  dmenu-any)
    selection="$(select_title_artist_album)"
    if [ "$selection" != "" ]; then
      mpc_file "${2:-play}" "$selection"
    fi
    ;;
  notifications-daemon)
    mpc idleloop player mixer | while read -r event; do
      case "$event" in
        player)   notify_current_status   ;;
        mixer)    notify_mpd_volume       ;;
      esac
    done
esac
