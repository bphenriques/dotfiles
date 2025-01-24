#shellcheck shell=bash

# Issues:
# - Applying to a class as whole does not work: brightnessctl --machine-readable --class=backlight set 10

OSD_BRIGHTNESS_OFF_ICON="${OSD_BRIGHTNESS_OFF_ICON:-}"
OSD_BRIGHTNESS_LOW_ICON="${OSD_BRIGHTNESS_LOW_ICON:-}"
OSD_BRIGHTNESS_MEDIUM_ICON="${OSD_BRIGHTNESS_MEDIUM_ICON:-}"
OSD_BRIGHTNESS_HIGH_ICON="${OSD_BRIGHTNESS_HIGH_ICON:-}"

# ignore 'kbd_backlight'
default_device() { brightnessctl --machine-readable | awk -F, '{ print $1; }'; }
list_backlight_devices() { brightnessctl --machine-readable -l | grep ',backlight,' | awk -F, '{ print $1; }'; }
get_percentage() { brightnessctl --device="${2:-"$(default_device)"}" --machine-readable | awk -F, '{print $4}' | tr -d %; }
set_brightness() { brightnessctl --device="${2:-"$(default_device)"}" set "$1" ; }

notify() {
  percentage="$1"
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
  increase)
    shift 1
    delta="+${1:-5}%"
    device="${2:-"$(default_device)"}"
    set_brightness "$delta" "$device"
    notify "$(get_percentage "$device")"
    ;;
  decrease)
    shift 1
    delta="${1:-5}-%"
    device="${2:-"$(default_device)"}"
    set_brightness "$delta" "$device"
    notify "$(get_percentage "$device")"
    ;;
  list) list_backlight_devices ;;
esac