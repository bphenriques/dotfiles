#shellcheck shell=bash

LAPTOP_UPOWER_DEVICE="${UPOWER_DEVICE:-battery_BAT0}"
UPOWER_NOTIFY_LOW="${UPOWER_NOTIFY_LOW:-30}"
UPOWER_NOTIFY_CRITICAL="${UPOWER_NOTIFY_CRITICAL:-20}"

is_plugged() { acpi -a | grep -q "on-line"; }
get_battery_percentage() { upower -i "/org/freedesktop/UPower/devices/${LAPTOP_UPOWER_DEVICE}" | grep percentage | awk '{ print $2; }' | tr -d '%'; }

#shellcheck disable=SC2034
monitor_battery_status() {
  echo "Monitoring battery capacity..."
  last_plugged=
  last_percentage=
  critical=0
  low=0
  upower --monitor | while read -r timestamp junk1 junk2 path; do
      [ "$path" != "/org/freedesktop/UPower/devices/$LAPTOP_UPOWER_DEVICE" ] && continue

      plugged=
      if is_plugged; then
        plugged=1
      else
        plugged=0
      fi
      percentage="$(get_battery_percentage)"

      if [ "$percentage" != "$last_percentage" ]; then
        echo "Change in capacity: $percentage"
        if [ "$percentage" -ge 0 ] && [ "$percentage" -le "$UPOWER_NOTIFY_CRITICAL" ] && [ "$critical" -eq 0 ]; then
          critical=1
          low=0
          notify_current_battery "$percentage"
        elif [ "$percentage" -ge "$UPOWER_NOTIFY_CRITICAL" ] && [ "$percentage" -le "$UPOWER_NOTIFY_LOW" ] && [ "$low" -eq 0 ]; then
          critical=0
          low=1
          notify_current_battery "$percentage"
        else
          critical=0
          low=0
        fi
        last_percentage="$percentage"
      elif [ "$plugged" != "$last_plugged" ]; then
        echo "Change in plug/unplug: $plugged"
        notify_current_battery "$percentage"
        last_plugged="$plugged"
      fi
    done
}

get_icon() {
  percentage="$1"
  if [ "$percentage" -ge 0 ] && [ "$percentage" -lt 10 ]; then
    echo "$UPOWER_NOTIFY_LEVEL_0_ICON"
  elif [ "$percentage" -ge 10 ] && [ "$percentage" -lt 20 ]; then
    echo "$UPOWER_NOTIFY_LEVEL_10_ICON"
  elif [ "$percentage" -ge 20 ] && [ "$percentage" -lt 30 ]; then
    echo "$UPOWER_NOTIFY_LEVEL_20_ICON"
  elif [ "$percentage" -ge 30 ] && [ "$percentage" -lt 40 ]; then
    echo "$UPOWER_NOTIFY_LEVEL_30_ICON"
  elif [ "$percentage" -ge 40 ] && [ "$percentage" -lt 50 ]; then
    echo "$UPOWER_NOTIFY_LEVEL_40_ICON"
  elif [ "$percentage" -ge 50 ] && [ "$percentage" -lt 60 ]; then
    echo "$UPOWER_NOTIFY_LEVEL_50_ICON"
  elif [ "$percentage" -ge 60 ] && [ "$percentage" -lt 70 ]; then
    echo "$UPOWER_NOTIFY_LEVEL_60_ICON"
  elif [ "$percentage" -ge 70 ] && [ "$percentage" -lt 80 ]; then
    echo "$UPOWER_NOTIFY_LEVEL_70_ICON"
  elif [ "$percentage" -ge 80 ] && [ "$percentage" -lt 90 ]; then
    echo "$UPOWER_NOTIFY_LEVEL_80_ICON"
  elif [ "$percentage" -ge 90 ] && [ "$percentage" -lt 100 ]; then
    echo "$UPOWER_NOTIFY_LEVEL_90_ICON"
  else
    echo "$UPOWER_NOTIFY_LEVEL_100_ICON"
  fi
}

get_charging_icon() {
  percentage="$1"
  if [ "$percentage" -ge 0 ] && [ "$percentage" -lt 10 ]; then
    echo "$UPOWER_NOTIFY_LEVEL_0_CHARGING_ICON"
  elif [ "$percentage" -ge 10 ] && [ "$percentage" -lt 20 ]; then
    echo "$UPOWER_NOTIFY_LEVEL_10_CHARGING_ICON"
  elif [ "$percentage" -ge 20 ] && [ "$percentage" -lt 30 ]; then
    echo "$UPOWER_NOTIFY_LEVEL_20_CHARGING_ICON"
  elif [ "$percentage" -ge 30 ] && [ "$percentage" -lt 40 ]; then
    echo "$UPOWER_NOTIFY_LEVEL_30_CHARGING_ICON"
  elif [ "$percentage" -ge 40 ] && [ "$percentage" -lt 50 ]; then
    echo "$UPOWER_NOTIFY_LEVEL_40_CHARGING_ICON"
  elif [ "$percentage" -ge 50 ] && [ "$percentage" -lt 60 ]; then
    echo "$UPOWER_NOTIFY_LEVEL_50_CHARGING_ICON"
  elif [ "$percentage" -ge 60 ] && [ "$percentage" -lt 70 ]; then
    echo "$UPOWER_NOTIFY_LEVEL_60_CHARGING_ICON"
  elif [ "$percentage" -ge 70 ] && [ "$percentage" -lt 80 ]; then
    echo "$UPOWER_NOTIFY_LEVEL_70_CHARGING_ICON"
  elif [ "$percentage" -ge 80 ] && [ "$percentage" -lt 90 ]; then
    echo "$UPOWER_NOTIFY_LEVEL_80_CHARGING_ICON"
  elif [ "$percentage" -ge 90 ] && [ "$percentage" -lt 100 ]; then
    echo "$UPOWER_NOTIFY_LEVEL_90_CHARGING_ICON"
  else
    echo "$UPOWER_NOTIFY_LEVEL_100_CHARGING_ICON"
  fi
}

notify_current_battery() {
  percentage="$1"

  urgency=
  if [ "$percentage" -ge 0 ] && [ "$percentage" -lt "$UPOWER_NOTIFY_CRITICAL" ]; then
    urgency=critical
  elif [ "$percentage" -ge "$UPOWER_NOTIFY_CRITICAL" ] && [ "$percentage" -lt "$UPOWER_NOTIFY_LOW" ]; then
    urgency=low
  else
    urgency=normal
  fi

  icon=
  if is_plugged; then
    icon="$(get_charging_icon "$percentage")"
  else
    icon="$(get_icon "$percentage")"
  fi

  notify-send \
    --urgency="$urgency" \
    --expire-time 5000 \
    --icon "$icon" \
    --category "upower-notify" \
    --hint string:x-canonical-private-synchronous:upower-notify \
    --hint string:x-dunst-stack-tag:upower-notify \
    --hint int:value:"$percentage" \
    --transient \
    "Battery: $percentage%"
}

case "${1:-}" in
  get-notify)           notify_current_battery "$(get_battery_percentage)"    ;;
  monitor-status)       monitor_battery_status                                ;;
esac