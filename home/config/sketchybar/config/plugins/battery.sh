#!/usr/bin/env sh
# shellcheck disable=SC1091
. "$HOME/.config/sketchybar/theme.sh"

PERCENTAGE="$(pmset -g batt | grep -Eo "\d+%" | cut -d% -f1)"
if [ "$(pmset -g batt | grep 'AC Power')" != "" ]; then
  ICON=""
  ICON_COLOR=$FOREGROUND
else
  case "$PERCENTAGE" in
    [8-9][0-9] | 100)
        ICON=""
        ICON_COLOR=$FOREGROUND
        ;;
    7[0-9])
        ICON=""
        ICON_COLOR=$FOREGROUND
        ;;
    [4-6][0-9])
        ICON=""
        ICON_COLOR=$FOREGROUND
        ;;
    [1-3][0-9])
        ICON=""
        ICON_COLOR=$FOREGROUND
        ;;
    [0-9])
        ICON=""
        ICON_COLOR=$CRITICAL
        ;;
  esac
fi

sketchybar --set "$NAME" icon="$ICON" icon.color="$ICON_COLOR" label="${PERCENTAGE}%"
