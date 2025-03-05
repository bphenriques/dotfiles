#shellcheck shell=bash

set_profile_profile() {
  if ! powerprofilesctl set "$1"; then
    notify-send \
      --urgency=critical \
      --expire-time 1500 \
      --icon "$POWER_PROFILE_ERROR_ICON" \
      --category "powerprofilesctl" \
      --hint string:x-canonical-private-synchronous:powerprofilesctl \
      --hint string:x-dunst-stack-tag:powerprofilesctl \
      "Power Profile" "Failed to set"
  fi
}

notify_current() {
  current="$(powerprofilesctl get)"
  icon=
  case "$current" in
    power-saver)  icon="$POWER_PROFILE_POWER_SAVER_ICON" ;;
    balanced)     icon="$POWER_PROFILE_BALANCED_ICON" ;;
    performance)  icon="$POWER_PROFILE_PERFORMANCE_ICON" ;;
  esac

  notify-send \
    --expire-time 1500 \
    --icon "$icon" \
    --category "powerprofilesctl" \
    --hint string:x-canonical-private-synchronous:powerprofilesctl \
    --hint string:x-dunst-stack-tag:powerprofilesctl \
    "Power Profile" "$current"
}

case "${1:-}" in
  set) shift 1 && set_profile_profile "$1" && notify_current  ;;
  get) shift 1 && notify_current                              ;;
esac