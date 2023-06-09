#!/usr/bin/env sh

# Assumes that whenever I turn bluetooth on is to connect to a headset (my use-case).
STATUS="$(system_profiler SPBluetoothDataType -json | jq -r '.SPBluetoothDataType[0]')"
BLUETOOTH_STATE="$(echo "${STATUS}" | jq -r '.controller_properties.controller_state')"

if [ "${BLUETOOTH_STATE}" = "attrib_on" ]; then
  DEVICE="$(echo "$STATUS" | jq -r '.device_connected // [] | .[] | to_entries[] | select((.value.device_minorType // "") = "Headset") | .key')"
  if [ "$DEVICE" = "" ]; then
    sketchybar --set "$NAME" icon=󰂯
  else
    case "$DEVICE" in
      "Galaxy Buds+ (9229)" ) ICON=󱡒 ;;
      "WH-1000XM2" ) ICON=󰋋 ;;
      * ) ICON=󰂱
    esac
    sketchybar --set "$NAME" icon=$ICON
  fi
else
  sketchybar --set "$NAME" icon=󰂲
fi
