#shellcheck shell=bash

_notify() { notify-send --category "screen-recorder" --hint string:x-canonical-private-synchronous:screen-recorder "$@"; }

is_recording() { pidof wl-screenrec >/dev/null; }

watch_stop() {
  local action
  action="$(_notify --wait --expire-time 0 --action "stop=Stop" --icon "$RECORD_ICON" "Screen Recorder" "Recording..." || true)"
  if [[ $action == "stop" ]]; then pkill --signal INT --exact wl-screenrec || true; fi
}

notify_saved() {
  local destination="$1" thumb action
  thumb="$(mktemp --suffix=.png)"
  if ffmpeg -y -loglevel error -i "$destination" -frames:v 1 "$thumb"; then
    action="$(_notify --action "default=Play" --expire-time 10000 --icon "$thumb" \
      "Screen Recorder" "$(basename "$destination")" 2>/dev/null || true)"
    if [[ $action == "default" ]]; then xdg-open "$destination" >/dev/null 2>&1 || true; fi
  else
    _notify --icon "$INFORMATION_ICON" "Screen Recorder" "Recording saved"
  fi
  rm -f -- "$thumb"
}

record() {
  local destination
  destination="$1/record-$(date +'%Y%m%d-%H%M%S').mp4"
  shift 1
  wl-screenrec --filename="$destination" --audio --audio-device "$AUDIO_DEVICE" "$@" &
  local recorder=$!
  watch_stop &

  local status=0
  wait "$recorder" || status=$?

  if [[ $status -eq 0 && -f $destination ]]; then
    notify_saved "$destination"
  else
    _notify --urgency=critical --icon "$ERROR_ICON" "Screen Recorder" "Recording failed"
  fi
}

stop() {
  if is_recording; then
    pkill --signal INT --exact wl-screenrec
  else
    _notify --icon "$INFORMATION_ICON" "Screen Recorder" "Nothing being recorded..."
  fi
}

if [[ ${1:-} == "screen" || ${1:-} == "region" ]] && is_recording; then
  stop
  exit 0
fi

case "${1:-}" in
  screen) record "${2:?}" ;;
  region)
    geometry="$(slurp -d)" || exit 0
    record "${2:?}" --geometry "$geometry"
    ;;
  stop) stop ;;
  *) echo "Usage: screen-recorder screen|region <directory> | stop" >&2 && exit 1 ;;
esac
