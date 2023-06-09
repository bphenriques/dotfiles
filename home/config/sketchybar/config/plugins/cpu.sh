#!/usr/bin/env sh

CPU_USAGE="$(iostat | awk 'FNR == 3 {print 100-$6}')"

sketchybar --set "$NAME" label="${CPU_USAGE}%"
