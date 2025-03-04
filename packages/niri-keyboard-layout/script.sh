#shellcheck shell=bash

notify_current() {
  case "$(niri msg --json keyboard-layouts | jq -r '.names[.current_idx]')" in
    "English"*)     notify-send "Keyboard Layout" "Set to US" ;;
    "Portuguese"*)  notify-send "Keyboard Layout" "Set to PT" ;;
  esac
}

case "${1:-}" in
  next) niri msg action switch-layout next && notify_current ;;
esac

