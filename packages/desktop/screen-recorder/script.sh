#shellcheck shell=bash

: "${XDG_RUNTIME_DIR:?XDG_RUNTIME_DIR must be set}"
STATE_FILE="$XDG_RUNTIME_DIR/screen-recorder-destination"

_notify() { notify-send --category "screen-recorder" --hint string:x-canonical-private-synchronous:screen-recorder "$@"; }

is_recording() { pidof wl-screenrec >/dev/null; }
record() {
  destination="$1/record-$(date +'%Y%m%d-%H%M%S').mp4"
  shift 1
  if is_recording; then
    _notify --icon "$ERROR_ICON" "Screen Recorder" "There is already a recording in progress"
    return 1
  else
    printf '%s' "$destination" > "$STATE_FILE"
    _notify --icon "$RECORD_ICON" "Screen Recorder" "Starting recording..."

    sleep 1
    if ! wl-screenrec --filename="${destination}" "$@"; then
      rm -f "$STATE_FILE"
      _notify --urgency=critical --icon "$ERROR_ICON" "Screen Recorder" "Failed to start recording"
    fi
  fi
}

stop() {
  if ! is_recording; then
    _notify --icon "$INFORMATION_ICON" "Screen Recorder" "Nothing being recorded..."
    return
  fi

  pkill --signal INT wl-screenrec
  sleep 0.5

  local destination=""
  if [[ -f "$STATE_FILE" ]]; then
    destination="$(cat "$STATE_FILE")"
    rm -f "$STATE_FILE"
  fi

  if [[ -n $destination && -f $destination ]]; then
    local thumb action
    thumb="$(mktemp --suffix=.png)"
    if ffmpeg -y -loglevel error -i "$destination" -frames:v 1 "$thumb"; then
      action=$(_notify \
        --action "default=Play" \
        --expire-time 10000 \
        --icon "$thumb" \
        "Screen Recorder" \
        "$(basename "$destination")" 2>/dev/null || true)

      if [ "$action" = "default" ]; then
        xdg-open "$destination" >/dev/null 2>&1 || true
      fi
    else
      _notify --icon "$INFORMATION_ICON" "Screen Recorder" "Recording saved"
    fi
    rm -f -- "$thumb"
  else
    _notify --icon "$INFORMATION_ICON" "Screen Recorder" "Recording stopped"
  fi
}

case "${1:-}" in
  screen-audio) shift 1 && record "$1" --audio ;;
  screen-no-audio) shift 1 && record "$1" ;;
  region-audio)
    shift 1 && geometry="$(slurp -d)" || exit 0
    record "$1" --geometry "$geometry" --audio
    ;;
  region-no-audio)
    shift 1 && geometry="$(slurp -d)" || exit 0
    record "$1" --geometry "$geometry"
    ;;
  stop) shift 1 && stop ;;
  *) echo "Unknown command" && exit 1 ;;
esac
