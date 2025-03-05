#shellcheck shell=bash

is_recording() { pidof wl-screenrec > /dev/null; }
record() {
  destination="$1/record-$(date +'%Y%m%d-%H%M%S').mp4"
  shift 1
  if is_recording; then
    notify-send \
      --expire-time 1500 \
      --icon "$ERROR_ICON" \
      --category "screen-recorder" \
      --hint string:x-canonical-private-synchronous:screen-recorder \
      --hint string:x-dunst-stack-tag:screen-recorder \
      "Screen Recorder" "There is already a recording in progress"

    return 1
  else
    notify-send \
      --expire-time 1000 \
      --icon "$RECORD_ICON" \
      --category "screen-recorder" \
      --hint string:x-canonical-private-synchronous:screen-recorder \
      --hint string:x-dunst-stack-tag:screen-recorder \
      "Screen Recorder" "Starting recording..."

    sleep 1
    if ! wl-screenrec --filename="${destination}" "$@"; then
      notify-send \
        --urgency=critical \
        --expire-time 1500 \
        --icon "$ERROR_ICON" \
        --category "screen-recorder" \
        --hint string:x-canonical-private-synchronous:screen-recorder \
        --hint string:x-dunst-stack-tag:screen-recorder \
        "Screen Recorder" "Couldn't start recording"
    fi
  fi
}

stop() {
  if is_recording; then
    pkill --signal INT wl-screenrec
    notify-send \
      --expire-time 1500 \
      --icon "$INFORMATION_ICON" \
      --category "screen-recorder" \
      --hint string:x-canonical-private-synchronous:screen-recorder \
      --hint string:x-dunst-stack-tag:screen-recorder \
      "Screen Recorder" "Recording stopped..."
  else
    notify-send \
      --expire-time 1000 \
      --icon "$INFORMATION_ICON" \
      --category "screen-recorder" \
      --hint string:x-canonical-private-synchronous:screen-recorder \
      --hint string:x-dunst-stack-tag:screen-recorder \
      "Screen Recorder" "Nothing being recorded..."
  fi
}

case "${1:-}" in
  screen-audio)     shift 1 && record "$1" --audio                              ;;
  screen-no-audio)  shift 1 && record "$1"                                      ;;
  region-audio)     shift 1 && record "$1" --geometry "$(slurp -d)" --audio     ;;
  region-no-audio)  shift 1 && record "$1" --geometry "$(slurp -d)"             ;;
  stop)             shift 1 && stop                                             ;;
  *)                echo "Unknown command" && exit 1                            ;;
esac
