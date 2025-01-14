#shellcheck shell=bash

# TODO: too tied to niri. We can check if the binary exists.
case "$(niri msg --json focused-window | jq -r '.app_id')" in
  "com.mitchellh.ghostty")  wtype -M shift -M ctrl v  ;;
  *)                        wtype -M ctrl v           ;;
esac