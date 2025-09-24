#shellcheck shell=bash

XDG_CONFIG_HOME="${XDG_CONFIG_HOME:-"$HOME"/.config}"
XDG_STATE_HOME="${XDG_STATE_HOME:-"$HOME"/.local/state}"
XDG_CACHE_HOME="${XDG_CACHE_HOME:-"$HOME"/.cache}"

MPC_PLUS_CACHE_DIR="${XDG_CACHE_HOME}/mpc-plus"
MPC_PLUS_CONFIG_DIR="${XDG_CONFIG_HOME}/mpc-plus"
MPC_PLUS_STATE_DIR="${XDG_STATE_HOME}/mpc-plus"
MPC_PLUS_SELECTED_HOST="${MPC_PLUS_STATE_DIR}/selected.env"

save_or_compress() { magick - -geometry x128 "$destination" >/dev/null 2>&1; }
_notify() { notify-send --category "mpc-plus" --hint string:x-canonical-private-synchronous:mpc-plus --transient "$@"; }

toggle_random() { case "$(mpc status '%random%')" in off) mpc random on ;; on) mpc random off ;; esac }
random_status_icon() { case "$(mpc status '%random%')" in off) echo -n "${MPC_PLUS_NO_RANDOM_ICON}" ;; on) echo -n "${MPC_PLUS_RANDOM_ICON}" ;; esac }

toggle_repeat() { case "$(mpc status '%repeat%')" in off) mpc repeat on ;; on) mpc repeat off ;; esac }
repeat_status_icon() { case "$(mpc status '%repeat%')" in off) echo -n "${MPC_PLUS_NO_REPEAT_ICON}" ;; on) echo -n "${MPC_PLUS_REPEAT_ICON}" ;; esac }

list_title_artist_album() {
  mpc -f '%file%\t%title%\t%artist%\t%album%' listall | while IFS=$'\t' read -r file title artist album; do
    # Minor safeguard and assumption: the library is organized using the standard (?) Artist/Album/Song. Works for me. Might not work with you.
    if [ -n "$title" ] && [ -n "$artist" ] && [ -n "$album" ]; then
      printf "%s\t%s\u0000icon\u001f%s\n" "$artist" "$artist" "$MPC_PLUS_ARTIST_ICON"
      printf "%s\t%s by %s\u0000icon\u001f%s\n" "$artist/$album" "$album" "$artist" "$MPC_PLUS_ALBUM_ICON"
      printf "%s\t%s by %s (%s)\u0000icon\u001f%s\n" "$file" "$title" "$artist" "$album" "$MPC_PLUS_SONG_ICON"
    else
      printf "%s\t%s\u0000icon\u001f%s\n" "$file" "$file" "$MPC_PLUS_SONG_ICON"
    fi
  done | LC_ALL=C sort --unique
}
select_title_artist_album() { list_title_artist_album | fuzzel --dmenu --with-nth=2 --accept-nth=1; }

# FIXME: Ideally we would get the file and check if the duration of the track is -1. This suffices _for me_.
# shellcheck disable=SC2016
list_radio_stream() { mpc lsplaylists | grep '^radio' | xargs -I{} sh -c 'printf "%s\t%s\n" {} "$(mpc playlist {})"' | LC_ALL=C sort --unique; }
select_radio_stream() { list_radio_stream | fuzzel --dmenu --accept-nth=1 --with-nth="2.."; }

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

handle_event() {
  local summary msg icon
  msg="$(mpc -f '[%title%[ by %artist%]]|%file%' current)"
  case "$(mpc status '%state%')" in
    playing)
      summary="Now playing"
      icon="$(get_mpc_artwork_icon)"
      ;;
    paused)
      summary="Paused"
      icon="$(get_mpc_artwork_icon)"
      ;;
    stopped)
      summary="Nothing playing"
      icon="${MPC_PLUS_STOPPED_ICON}"
      ;;
  esac

  case "$1" in
    player) _notify -i "${icon}" "$summary" "$msg"                                                ;;
    mixer)  _notify -i "${icon}" -h "int:value:$(mpc volume | grep -Po '[\d]+')" "$summary" "$msg" ;;
  esac
}

# shellcheck source=/dev/null
load_config() {
  mkdir -p "${MPC_PLUS_CONFIG_DIR}"
  mkdir -p "${MPC_PLUS_CACHE_DIR}"
  mkdir -p "${MPC_PLUS_STATE_DIR}"

  if [ ! -f "${MPC_PLUS_CONFIG_DIR}"/default.json ]; then
    echo '{ "host": "localhost", "port": 6600 }' > "${MPC_PLUS_CONFIG_DIR}"/default.json
  fi

  if [ ! -f "${MPC_PLUS_SELECTED_HOST}" ]; then
    config_set_server default
  fi

  set -o allexport
  source "${MPC_PLUS_SELECTED_HOST}"
  set +o allexport
}

config_list_servers() { find -L "${MPC_PLUS_CONFIG_DIR}" -type f -name '*.json' ! -name 'config.json' -exec basename {} ".json" \;; }
config_set_server() {
  local selected="$1"
  local device_file="${MPC_PLUS_CONFIG_DIR}/$selected.json";
  if [ ! -f "${device_file}" ]; then
    _notify --urgency=critical --icon "$MPC_PLUS_ERROR_ICON" "No configuration file available for $selected"
  fi

  printf 'MPD_HOST_DISPLAY_NAME="%s"\nMPD_HOST="%s"\nMPD_PORT="%s"' \
    "$selected" "$(jq -cr '.host' < "${device_file}")" "$(jq -cr '.port' \
    < "${device_file}")" \
    > "${MPC_PLUS_SELECTED_HOST}"
}

load_config
case "${1:-}" in
  play-pause)       mpc toggle                                          ;;
  stop)             mpc stop                                            ;;
  previous)         mpc prev                                            ;;
  next)             mpc next                                            ;;
  volume-increase)  mpc volume "+${2:-5}"                               ;;
  volume-decrease)  mpc volume "-${2:-5}"                               ;;
  play-shuffled)    mpc random on && mpc clear && mpc add / && mpc play ;;
  clear)            mpc clear && _notify -i "${MPC_PLUS_CLEAR_ICON}" "Music Queue" "No songs"                       ;;
  toggle-random)    toggle_random && _notify -i "$(random_status_icon)" "Shuffle Songs: $(mpc status '%random%')"   ;;
  toggle-repeat)    toggle_repeat && _notify -i "$(repeat_status_icon)" "Repeat Song: $(mpc status '%repeat%')"     ;;
  dmenu-file-exec)
    selection="$(select_title_artist_album)"
    if [ "$selection" != "" ]; then
      case "${2:-play}" in
        play)     mpc clear && mpc add "$selection" && mpc play ;;
        next)     mpc insert "$selection" && mpc play           ;;
        add)      mpc add "$selection" && mpc play              ;;
      esac
    fi
    ;;
  dmenu-radio-stream)
    selection="$(select_radio_stream)"
    if [ "$selection" != "" ]; then
      mpc clear && mpc load "${selection}" && mpc play
    fi
    ;;
  list-servers)         config_list_servers ;;
  dmenu-select-server)
    selection="$(config_list_servers | fuzzel --dmenu --placeholder "Current: ${MPD_HOST_DISPLAY_NAME}")"
    if [ "$selection" != "" ]; then
      config_set_server "$selection"
    fi
    _notify -i "${MPC_PLUS_DEVICE_ICON}" "MPD Server: $selection"
    ;;
  notifications-daemon) mpc idleloop player mixer | while read -r event; do handle_event "$event"; done  ;;
esac
