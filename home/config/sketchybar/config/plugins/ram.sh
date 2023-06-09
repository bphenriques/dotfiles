#!/usr/bin/env sh

MEMORY_PERCENTAGE="$(memory_pressure | grep "System-wide memory free percentage:" | awk '{ printf("%02.0f\n", 100-$5"%") }')"

sketchybar --set "$NAME" label="${MEMORY_PERCENTAGE}%"
