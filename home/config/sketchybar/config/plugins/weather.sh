#!/usr/bin/env sh

LOCATION=Lisbon

update() {
  WEATHER="$(curl "wttr.in/$LOCATION?format=%c%t+")"
  if test -z "$WEATHER"; then
      sketchybar --set "$NAME" drawing=off
      return
  fi

  sketchybar --set "$NAME" label="$WEATHER" drawing=on
}

update
