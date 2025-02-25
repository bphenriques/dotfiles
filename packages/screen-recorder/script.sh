#shellcheck shell=bash

is_recording() { pidof wl-screenrec > /dev/null; }
record() {
  destination="$1"
  shift 1
  if is_recording; then
    notify-send "Video record" "There is already a recording in progress"
    return 1
  else
    notify-send "Video record" "Starting recording..." --expire-time=1000
    sleep 1
    wl-screenrec --filename="${destination}" "$@"
  fi
}

stop() {
  if is_recording; then
    pkill --signal INT wl-screenrec
    notify-send "Video record" "Recording stopped..."
  else
    notify-send "Video record" "Nothing being recorded..."
  fi
}

# TODO: interesting: https://github.com/russelltg/wl-screenrec/issues/66
case "${1:-}" in
  screen-audio)     shift 1 && record "$1" --audio                              ;;
  screen-no-audio)  shift 1 && record "$1"                                      ;;
  region-audio)     shift 1 && record "$1" --geometry "$(slurp -d)" --audio     ;;
  region-no-audio)  shift 1 && record "$1" --geometry "$(slurp -d)"             ;;
  stop)             shift 1 && stop                                             ;;
  *)                echo "Unknown command" && exit 1                            ;;
esac
