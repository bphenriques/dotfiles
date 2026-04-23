#shellcheck shell=bash

XDG_CONFIG_HOME="${XDG_CONFIG_HOME:-"$HOME"/.config}"
XDG_STATE_HOME="${XDG_STATE_HOME:-"$HOME"/.local/state}"
XDG_CACHE_HOME="${XDG_CACHE_HOME:-"$HOME"/.cache}"

MPC_PLUS_CACHE_DIR="${XDG_CACHE_HOME}/mpc-plus"
MPC_PLUS_CONFIG_DIR="${XDG_CONFIG_HOME}/mpc-plus"
MPC_PLUS_STATE_DIR="${XDG_STATE_HOME}/mpc-plus"
MPC_PLUS_SELECTED_HOST="${MPC_PLUS_STATE_DIR}/selected.env"

_notify() { notify-send --category "mpc-plus" --hint string:x-canonical-private-synchronous:mpc-plus --transient "$@"; }

toggle_random() { case "$(mpc status '%random%')" in off) mpc random on >/dev/null && echo on ;; on) mpc random off >/dev/null && echo off ;; esac }
toggle_repeat() { case "$(mpc status '%repeat%')" in off) mpc repeat on >/dev/null && echo on ;; on) mpc repeat off >/dev/null && echo off ;; esac }

list_title_artist_album() {
  mpc -f '%file%\t%title%\t%artist%\t%album%' listall | while IFS=$'\t' read -r file title artist album; do
    # Minor safeguard and assumption: the library is organized using the standard (?) Artist/Album/Song. Works for me. Might not work with you.
    if [[ -n $title && -n $artist && -n $album ]]; then
      printf "%s\t%s\u0000icon\u001f%s\n" "$artist" "$artist" "$MPC_PLUS_ARTIST_ICON"
      printf "%s\t%s by %s\u0000icon\u001f%s\n" "$artist/$album" "$album" "$artist" "$MPC_PLUS_ALBUM_ICON"
      printf "%s\t%s by %s (%s)\u0000icon\u001f%s\n" "$file" "$title" "$artist" "$album" "$MPC_PLUS_SONG_ICON"
    else
      printf "%s\t%s\u0000icon\u001f%s\n" "$file" "$file" "$MPC_PLUS_SONG_ICON"
    fi
  done | LC_ALL=C sort --unique
}
select_title_artist_album() { list_title_artist_album | fuzzel --dmenu --with-nth=2 --accept-nth=1 --counter --placeholder "Search by title, artist, or album..."; }

list_radio_stream() {
  mpc lsplaylists | while read -r pl; do
    [[ $pl == radio* ]] || continue
    printf "%s\t%s\n" "$pl" "$pl"
  done | LC_ALL=C sort --unique
}
select_radio_stream() { list_radio_stream | fuzzel --dmenu --accept-nth=1 --with-nth="2.."; }

# Resolves album artwork for a file path, using album-level caching.
# Skips streams, writes atomically via mktemp+mv.
# Runs in a subshell with pipefail so a failing mpc doesn't get masked by magick's exit code.
_try_mpc_art() {
  local cmd="$1" file="$2" out="$3"
  (
    set -o pipefail
    mpc "$cmd" "$file" 2>/dev/null | magick - -geometry x128 "PNG:$out" >/dev/null 2>&1
  ) && [[ -s $out ]]
}

get_mpc_artwork_icon() {
  local file="$1" album_dir key destination tmp

  [[ -n $file && $file != *://* ]] || {
    printf '%s' "$MPC_PLUS_SONG_ICON"
    return
  }

  # Cache key is album-level (directory). For per-track artwork, replace $album_dir with $file.
  album_dir="${file%/*}"
  key="$(printf '%s\0%s\0%s' "$MPD_HOST" "$MPD_PORT" "$album_dir" | sha256sum | cut -d' ' -f1)"
  destination="$MPC_PLUS_CACHE_DIR/$key.png"

  [[ -s $destination ]] && {
    printf '%s' "$destination"
    return
  }

  tmp="$(mktemp "$MPC_PLUS_CACHE_DIR/.art.XXXXXX.png")" || {
    printf '%s' "$MPC_PLUS_SONG_ICON"
    return
  }

  if _try_mpc_art readpicture "$file" "$tmp" || _try_mpc_art albumart "$file" "$tmp"; then
    mv -f "$tmp" "$destination"
    printf '%s' "$destination"
  else
    rm -f "$tmp"
    printf '%s' "$MPC_PLUS_SONG_ICON"
  fi
}

# In-memory state for the notifications-daemon, refreshed on player events and reused on mixer events.
CURRENT_SUMMARY="Nothing playing"
CURRENT_MSG=""
CURRENT_ICON="$MPC_PLUS_STOPPED_ICON"

refresh_current_state() {
  local file title artist name state

  state="$(mpc status '%state%' 2>/dev/null || true)"
  case "$state" in
    playing) CURRENT_SUMMARY="Now playing" ;;
    paused) CURRENT_SUMMARY="Paused" ;;
    *)
      CURRENT_SUMMARY="Nothing playing"
      CURRENT_MSG=""
      CURRENT_ICON="$MPC_PLUS_STOPPED_ICON"
      return
      ;;
  esac

  IFS=$'\t' read -r file title artist name < <(mpc current -f '%file%\t%title%\t%artist%\t%name%' 2>/dev/null || true)

  if [[ -n $title ]]; then
    CURRENT_MSG="$title${artist:+ by $artist}"
  elif [[ -n $name ]]; then
    CURRENT_MSG="$name"
  elif [[ -n $file ]]; then
    local basename="${file##*/}"
    CURRENT_MSG="${basename%.*}"
  else
    CURRENT_MSG=""
  fi

  CURRENT_ICON="$(get_mpc_artwork_icon "$file")"
}

handle_event() {
  case "$1" in
    player)
      refresh_current_state
      _notify -i "$CURRENT_ICON" "$CURRENT_SUMMARY" "$CURRENT_MSG"
      ;;
    mixer)
      local volume
      volume="$(mpc volume | grep -Eo '[0-9]+' || true)" # mpc volume may return 'n/a' when unavailable
      if [[ -n $volume ]]; then
        _notify -i "$CURRENT_ICON" -h "int:value:${volume}" "$CURRENT_SUMMARY" "$CURRENT_MSG"
      else
        _notify -i "$CURRENT_ICON" "$CURRENT_SUMMARY" "$CURRENT_MSG"
      fi
      ;;
  esac
}

# shellcheck source=/dev/null
load_config() {
  mkdir -p "${MPC_PLUS_CONFIG_DIR}" "${MPC_PLUS_CACHE_DIR}" "${MPC_PLUS_STATE_DIR}"

  # Use pre-determined device if set (e.g., from systemd per-service environment)
  if [[ -n ${MPC_PLUS_SINGLE_DEVICE:-} ]]; then
    local target="${MPC_PLUS_CONFIG_DIR}/${MPC_PLUS_SINGLE_DEVICE}.json"
    if [[ -f $target ]]; then
      MPD_HOST="$(jq -r '.host' <"$target")"
      MPD_PORT="$(jq -r '.port' <"$target")"
      MPD_HOST_DISPLAY_NAME="$MPC_PLUS_SINGLE_DEVICE"
    else
      printf "mpc-plus: device '%s' is not configured (%s)\n" "$MPC_PLUS_SINGLE_DEVICE" "$target" >&2
      return 1
    fi
  elif [[ -f ${MPC_PLUS_SELECTED_HOST} ]]; then
    source "${MPC_PLUS_SELECTED_HOST}"
  else
    MPD_HOST="${MPD_HOST:-"127.0.0.1"}"
    MPD_PORT=${MPD_PORT:-6600}
    MPD_HOST_DISPLAY_NAME="local"
  fi

  export MPD_HOST MPD_PORT
}

config_list_servers() { find -L "${MPC_PLUS_CONFIG_DIR}" -type f -name '*.json' ! -name 'config.json' -exec basename {} ".json" \;; }
config_set_server() {
  local selected="$1"
  local device_file="${MPC_PLUS_CONFIG_DIR}/$selected.json"
  if [[ ! -f ${device_file} ]]; then
    _notify --urgency=critical --icon "$MPC_PLUS_ERROR_ICON" "No configuration file available for $selected"
    return 1
  fi

  printf 'MPD_HOST_DISPLAY_NAME="%s"\nMPD_HOST="%s"\nMPD_PORT="%s"' \
    "$selected" "$(jq -cr '.host' <"${device_file}")" "$(jq -cr '.port' \
      <"${device_file}")" \
    >"${MPC_PLUS_SELECTED_HOST}"
}

load_config
case "${1:-}" in
  play-pause) mpc toggle ;;
  stop) mpc stop ;;
  previous) mpc prev ;;
  next) mpc next ;;
  volume-increase) mpc volume "+${2:-5}" ;;
  volume-decrease) mpc volume "-${2:-5}" ;;
  play-shuffled) mpc random on && mpc clear && mpc add / && mpc play ;;
  clear) mpc clear && _notify -i "${MPC_PLUS_CLEAR_ICON}" "Music Queue" "No songs" ;;
  toggle-random)
    new_state="$(toggle_random)"
    case "$new_state" in
      on) _notify -i "${MPC_PLUS_RANDOM_ICON}" "Shuffle Songs: on" ;;
      off) _notify -i "${MPC_PLUS_NO_RANDOM_ICON}" "Shuffle Songs: off" ;;
    esac
    ;;
  toggle-repeat)
    new_state="$(toggle_repeat)"
    case "$new_state" in
      on) _notify -i "${MPC_PLUS_REPEAT_ICON}" "Repeat Song: on" ;;
      off) _notify -i "${MPC_PLUS_NO_REPEAT_ICON}" "Repeat Song: off" ;;
    esac
    ;;
  dmenu-file-exec)
    selection="$(select_title_artist_album)"
    if [[ -n $selection ]]; then
      case "${2:-play}" in
        play) mpc clear && mpc add "$selection" && mpc play ;;
        next) mpc insert "$selection" && mpc play ;;
        add) mpc add "$selection" && mpc play ;;
      esac
    fi
    ;;
  dmenu-radio-stream)
    selection="$(select_radio_stream)"
    if [[ -n $selection ]]; then
      mpc clear && mpc load "${selection}" && mpc play
    fi
    ;;
  list-servers) config_list_servers ;;
  dmenu-select-server)
    selection="$(config_list_servers | fuzzel --dmenu --mesg "Current: ${MPD_HOST_DISPLAY_NAME}")"
    if [[ -n $selection ]]; then
      config_set_server "$selection"
      _notify -i "${MPC_PLUS_DEVICE_ICON}" "MPD Server: $selection"
    fi
    ;;
  # Long-lived daemon: process substitution (< <(...)) keeps the loop in the main shell so CURRENT_* globals persist.
  # mpc idleloop blocks until an event occurs, so no busy-looping. Outer loop reconnects if MPD restarts.
  notifications-daemon)
    while :; do
      refresh_current_state
      while IFS= read -r event; do
        handle_event "$event"
      done < <(mpc idleloop player mixer 2>/dev/null)
      sleep 1
    done
    ;;
esac
