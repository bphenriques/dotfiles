#shellcheck shell=bash
case "$(niri msg --json focused-window | jq -r '.app_id')" in
  "com.mitchellh.ghostty")  wtype -M shift -M ctrl v  ;;
  *)                        wtype -M ctrl v           ;;
esac