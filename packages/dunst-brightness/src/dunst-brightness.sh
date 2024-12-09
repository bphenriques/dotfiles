#shellcheck shell=sh

DUNST_BRIGHTNESS_OFF_ICON="${DUNST_BRIGHTNESS_OFF_ICON:-}"
DUNST_BRIGHTNESS_LOW_ICON="${DUNST_BRIGHTNESS_LOW_ICON:-}"
DUNST_BRIGHTNESS_MEDIUM_ICON="${DUNST_BRIGHTNESS_MEDIUM_ICON:-}"
DUNST_BRIGHTNESS_HIGH_ICON="${DUNST_BRIGHTNESS_HIGH_ICON:-}"

get_percentage() { brightnessctl -m | awk -F, '{print $4}' | tr -d %; }
delta() { brightnessctl set "$1" > /dev/null; }

notify() {
  percentage="$(get_percentage)"
  icon=
  if [ "$percentage" -eq 0 ]; then
    icon="$DUNST_BRIGHTNESS_OFF_ICON"
    progress=0
  elif [ "$percentage" -lt 30 ]; then
    icon="$DUNST_BRIGHTNESS_LOW_ICON"
    progress="$percentage"
  elif [ "$percentage" -lt 70 ]; then
    icon="$DUNST_BRIGHTNESS_MEDIUM_ICON"
    progress="$percentage"
  else
    icon="$DUNST_BRIGHTNESS_HIGH_ICON"
    progress="$percentage"
  fi

  dunstify \
    --timeout 1500 \
    --appname "brightness-osd" \
    --hints string:x-canonical-private-synchronous:brightness \
    --hints string:x-dunst-stack-tag:brightness \
    --hints int:value:"$progress" \
    --icon "$icon" \
    "Brightness: $progress%"
}

case "${1:-}" in
  increase)     shift 1 && delta "+${1:-10}%" && notify   ;;
  decrease)     shift 1 && delta "${1:-10}-%" && notify   ;;
esac
