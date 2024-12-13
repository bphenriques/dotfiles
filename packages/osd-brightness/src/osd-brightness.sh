#shellcheck shell=sh

OSD_BRIGHTNESS_OFF_ICON="${OSD_BRIGHTNESS_OFF_ICON:-}"
OSD_BRIGHTNESS_LOW_ICON="${OSD_BRIGHTNESS_LOW_ICON:-}"
OSD_BRIGHTNESS_MEDIUM_ICON="${OSD_BRIGHTNESS_MEDIUM_ICON:-}"
OSD_BRIGHTNESS_HIGH_ICON="${OSD_BRIGHTNESS_HIGH_ICON:-}"

get_percentage() { brightnessctl -m | awk -F, '{print $4}' | tr -d %; }
delta() { brightnessctl set "$1" > /dev/null; }

notify() {
  percentage="$(get_percentage)"
  icon=
  if [ "$percentage" -eq 0 ]; then
    icon="$OSD_BRIGHTNESS_OFF_ICON"
    progress=0
  elif [ "$percentage" -lt 30 ]; then
    icon="$OSD_BRIGHTNESS_LOW_ICON"
    progress="$percentage"
  elif [ "$percentage" -lt 70 ]; then
    icon="$OSD_BRIGHTNESS_MEDIUM_ICON"
    progress="$percentage"
  else
    icon="$OSD_BRIGHTNESS_HIGH_ICON"
    progress="$percentage"
  fi

  notify-send \
    --expire-time 1500 \
    --icon "$icon" \
    --category "brightness-osd" \
    --hint string:x-canonical-private-synchronous:brightness \
    --hint string:x-dunst-stack-tag:brightness \
    --hint int:value:"$progress" \
    "Brightness: $progress%"
}

case "${1:-}" in
  increase)     shift 1 && delta "+${1:-10}%" && notify   ;;
  decrease)     shift 1 && delta "${1:-10}-%" && notify   ;;
esac
