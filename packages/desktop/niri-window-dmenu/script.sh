#shellcheck shell=bash

windows="$(niri msg --json windows | jq -r -c "sort_by(.is_focused)")"

selection=
if [ "$(echo "$windows" | jq length)" -gt 2 ]; then
  selection="$(echo "${windows}" \
    | jq -r '.[] | "\(.app_id) - \(.title)\u0000icon\u001f\(.app_id)"' \
    | fuzzel --dmenu --index --width 65 --lines "$(echo "$windows" | jq length)"
  )"
else
  selection=0   # Automatically select the other window if there are two: the current active and the other one.
fi

if [ "$selection" != -1 ]; then
  niri msg action focus-window --id "$(echo "${windows}" | jq -c --arg INDEX "$selection" '.[$INDEX | tonumber].id')"
fi