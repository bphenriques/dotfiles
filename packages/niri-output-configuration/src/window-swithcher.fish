#!/usr/bin/env fish
set windows (niri msg --json windows)
and set id (echo $windows |
  jq -r '.[] | "\(.id): \(.title)\u0000icon\u001f\(.app_id)"' |
  string replace -ar [·—] - | # fuzzel ignores lines containing these characters for some reason
  fuzzel --dmenu --counter --width 65 --lines (echo $windows | jq length) |
  string split : -f 1)
and niri msg action focus-window --id $id