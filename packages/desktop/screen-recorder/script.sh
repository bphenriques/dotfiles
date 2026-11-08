#shellcheck shell=bash

_notify() { notify-send --category "screen-recorder" --hint string:x-canonical-private-synchronous:screen-recorder "$@"; }

STATE_FILE="${XDG_RUNTIME_DIR:?}/screen-recorder"
LOCK_FILE="${XDG_RUNTIME_DIR:?}/screen-recorder.lock"

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

# Runs holding the lock on fd 9, which wl-screenrec inherits: it stays held until the recorder has finished saving.
record() {
  local destination
  destination="$1/record-$(date +'%Y%m%d-%H%M%S').mp4"
  shift 1
  wl-screenrec --filename="$destination" --audio --audio-device "$AUDIO_DEVICE" "$@" &
  local recorder=$!
  date +%s >"$STATE_FILE" # in place: replacing the file would drop the Quickshell watch
  trap ': >"$STATE_FILE"' EXIT

  local status=0
  wait "$recorder" || status=$?
  : >"$STATE_FILE"
  trap - EXIT
  exec 9>&- # the next recording may start while the notification below waits

  if [[ $status -eq 0 && -f $destination ]]; then
    notify_saved "$destination"
  else
    _notify --urgency=critical --icon "$ERROR_ICON" "Screen Recorder" "Recording failed"
  fi
}

# Hides the pill at once; a recorder that has not finished saving within 5 seconds is hung, and is killed.
stop() {
  if ! pidof wl-screenrec >/dev/null; then
    _notify --icon "$INFORMATION_ICON" "Screen Recorder" "Nothing being recorded..."
    return
  fi
  : >"$STATE_FILE"
  pkill --signal INT --exact wl-screenrec
  timeout 5 pidwait --exact wl-screenrec || pkill --signal KILL --exact wl-screenrec
}

# Start, or stop the running recording; a press while one is still saving only says so, and one during region selection does nothing.
toggle() {
  exec 9>"$LOCK_FILE"
  if ! flock --nonblock 9; then
    if [[ -s $STATE_FILE ]]; then
      stop
    elif pidof wl-screenrec >/dev/null; then
      _notify --icon "$INFORMATION_ICON" "Screen Recorder" "Still saving the last recording..."
    fi
    exit 0
  fi
}

case "${1:-}" in
  screen)
    toggle
    record "${2:?}"
    ;;
  region)
    toggle
    geometry="$(slurp -d)" || exit 0
    record "${2:?}" --geometry "$geometry"
    ;;
  stop) stop ;;
  *) echo "Usage: screen-recorder screen|region <directory> | stop" >&2 && exit 1 ;;
esac
