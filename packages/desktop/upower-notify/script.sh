#shellcheck shell=bash

readonly UPOWER_DEVICE="${UPOWER_DEVICE:?'UPOWER_DEVICE must be set (e.g., battery_BAT0)'}"
readonly UPOWER_NOTIFY_LOW="${UPOWER_NOTIFY_LOW:-30}"
readonly UPOWER_NOTIFY_CRITICAL="${UPOWER_NOTIFY_CRITICAL:-20}"
readonly POLL_TIMEOUT=60
readonly UPOWER_DEVICE_PATH="/org/freedesktop/UPower/devices/${UPOWER_DEVICE}"

# Fail fast if the battery device does not exist
if ! upower -i "$UPOWER_DEVICE_PATH" &>/dev/null; then
  echo "Battery device '${UPOWER_DEVICE}' not found. Available devices:" >&2
  upower -e >&2
  exit 1
fi

is_plugged() { ! upower -i "$UPOWER_DEVICE_PATH" | grep -q "state:.*discharging"; }
get_battery_percentage() { upower -i "$UPOWER_DEVICE_PATH" | grep percentage | awk '{ print $2; }' | tr -d '%'; }

get_icon() {
  local level=$(( ($1 / 10) * 10 ))
  [ "$level" -gt 100 ] && level=100
  echo "$UPOWER_ICONS_DIR/$level"
}

get_charging_icon() {
  local level=$(( ($1 / 10) * 10 ))
  [ "$level" -gt 100 ] && level=100
  echo "$UPOWER_CHARGING_ICONS_DIR/$level"
}

notify_current_battery() {
  local percentage="$1"

  local urgency=
  if [ "$percentage" -ge 0 ] && [ "$percentage" -lt "$UPOWER_NOTIFY_CRITICAL" ]; then
    urgency=critical
  elif [ "$percentage" -ge "$UPOWER_NOTIFY_CRITICAL" ] && [ "$percentage" -lt "$UPOWER_NOTIFY_LOW" ]; then
    urgency=low
  else
    urgency=normal
  fi

  local icon=
  if is_plugged; then
    icon="$(get_charging_icon "$percentage")"
  else
    icon="$(get_icon "$percentage")"
  fi

  notify-send \
    --urgency="$urgency" \
    --icon "$icon" \
    --category "upower-notify" \
    --hint string:x-canonical-private-synchronous:upower-notify \
    --hint int:value:"$percentage" \
    --transient \
    "Battery: $percentage%"
}

# Reads current battery state, compares against last_state, sends notifications on threshold crossings or plug/unplug events, and updates last_state.
#shellcheck disable=SC2178,SC2128
check_and_notify() {
  local -n last_state=$1

  local percentage
  if ! percentage="$(get_battery_percentage)"; then
    echo "Failed to read battery percentage, skipping" >&2
    return 1
  fi
  local plugged=0
  is_plugged && plugged=1

  # Detect plug/unplug change
  if [ "$plugged" != "${last_state[plugged]}" ]; then
    notify_current_battery "$percentage"
  # Detect threshold crossings
  elif [ "$percentage" != "${last_state[percentage]}" ]; then
    if [ "$percentage" -le "$UPOWER_NOTIFY_CRITICAL" ] && [ "${last_state[notified_critical]}" -eq 0 ]; then
      last_state[notified_critical]=1
      last_state[notified_low]=0
      notify_current_battery "$percentage"
    elif [ "$percentage" -gt "$UPOWER_NOTIFY_CRITICAL" ] && [ "$percentage" -le "$UPOWER_NOTIFY_LOW" ] && [ "${last_state[notified_low]}" -eq 0 ]; then
      last_state[notified_critical]=0
      last_state[notified_low]=1
      notify_current_battery "$percentage"
    elif [ "$percentage" -gt "$UPOWER_NOTIFY_LOW" ]; then
      last_state[notified_critical]=0
      last_state[notified_low]=0
    fi
  fi

  last_state[percentage]="$percentage"
  last_state[plugged]="$plugged"
}

upower_monitor() {
  declare -A last_state=(
    [plugged]=0
    [percentage]=""
    [notified_low]=0
    [notified_critical]=0
  )

  # Seed state (no notification on startup)
  if ! last_state[percentage]="$(get_battery_percentage)"; then
    echo "Failed to get initial battery percentage, exiting" >&2
    exit 1
  fi
  is_plugged && last_state[plugged]=1

  # Pre-classify so we don't re-notify for the current state
  if [ "${last_state[percentage]}" -le "$UPOWER_NOTIFY_CRITICAL" ]; then
    last_state[notified_critical]=1
  elif [ "${last_state[percentage]}" -le "$UPOWER_NOTIFY_LOW" ]; then
    last_state[notified_low]=1
  fi

  echo "Monitoring battery (device=$UPOWER_DEVICE, percentage=${last_state[percentage]}%, plugged=${last_state[plugged]})"

  while true; do
    if read -t "$POLL_TIMEOUT" -r line; then
      case "$line" in
        *"$UPOWER_DEVICE_PATH"*) ;;
        *) continue ;;
      esac
    else
      # Timeout (rc > 128): no event received, fall through to poll battery state.
      # EOF (rc <= 128): pipe closed (upower died), exit so systemd restarts the service.
      rc=$?
      [ "$rc" -le 128 ] && { echo "upower --monitor pipe closed (rc=$rc), exiting" >&2; exit 1; }
    fi

    check_and_notify last_state || continue
  done
}

case "${1:-}" in
  notify)    notify_current_battery "$(get_battery_percentage)"  ;;
  monitor)   upower --monitor | upower_monitor                   ;;
esac
