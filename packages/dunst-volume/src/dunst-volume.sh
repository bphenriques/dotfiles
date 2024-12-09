#shellcheck shell=sh

DUNST_VOLUME_MUTED_ICON="${DUNST_VOLUME_MUTED_ICON:-}"
DUNST_VOLUME_LOW_ICON="${DUNST_VOLUME_LOW_ICON:-}"
DUNST_VOLUME_MEDIUM_ICON="${DUNST_VOLUME_MEDIUM_ICON:-}"
DUNST_VOLUME_HIGH_ICON="${DUNST_VOLUME_HIGH_ICON:-}"

get_percentage() { ponymix get-volume; }
is_muted() { ponymix is-muted; }
increase() { ponymix increase "$1"; }
decrease() { ponymix decrease "$1"; }
mute() { ponymix mute; }
unmute() { ponymix unmute; }
toggle_mute() { ponymix toggle; }
list() { ponymix list; }
set_default() { ponymix set-default -d "$1"; }
get_default_sink_name() { ponymix default | grep sink | awk '{print $3; }'; }

notify() {
  percentage="$(get_percentage)"
  icon=
  if is_muted || [ "$percentage" -eq 0 ]; then
    icon="$DUNST_VOLUME_MUTED_ICON"
    progress=0
  elif [ "$percentage" -lt 30 ]; then
    icon="$DUNST_VOLUME_LOW_ICON"
    progress="$percentage"
  elif [ "$percentage" -lt 70 ]; then
    icon="$DUNST_VOLUME_MEDIUM_ICON"
    progress="$percentage"
  else
    icon="$DUNST_VOLUME_HIGH_ICON"
    progress="$percentage"
  fi

  dunstify \
    --timeout 1500 \
    --appname "volume-osd" \
    --hints string:x-canonical-private-synchronous:volume \
    --hints string:x-dunst-stack-tag:volume \
    --hints int:value:"$progress" \
    --icon "$icon" \
    "Volume: $progress%"
}

case "${1:-}" in
  mute)         mute && notify                            ;;
  unmute)       unmute && notify                          ;;
  toggle-mute)  toggle_mute && notify                     ;;
  increase)     shift 1 && increase "${1:-10}" && notify  ;;
  decrease)     shift 1 && decrease "${1:-10}" && notify  ;;
  list)         shift 1 && list "$1"                      ;;
  set)          shift 1 && set_default "$1"               ;;
esac