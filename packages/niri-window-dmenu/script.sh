#shellcheck shell=bash

windows="$(niri msg --json windows | jq -r -c "sort_by(.is_focused)")"
selection="$(echo "${windows}" \
  | jq -r '.[] | "\(.app_id) - \(.title)\u0000icon\u001f\(.app_id)"' \
  | fuzzel --dmenu --index --width 65 --lines "$(echo "$windows" | jq length)"
)"

if [ "$selection" != -1 ]; then
  niri msg action focus-window --id "$(echo "${windows}" | jq -c --arg INDEX "$selection" '.[$INDEX | tonumber].id')"
fi