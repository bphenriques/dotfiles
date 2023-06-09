#!/usr/bin/env sh
# shellcheck disable=SC1091
. "$HOME/.config/sketchybar/theme.sh"

if [ "$SELECTED" = "true" ]; then
  sketchybar --set "$NAME" icon.color="$BLACK" background.color="$YELLOW"
else
  sketchybar --set "$NAME" icon.color="$FOREGROUND" background.color="$GRAY"
fi
