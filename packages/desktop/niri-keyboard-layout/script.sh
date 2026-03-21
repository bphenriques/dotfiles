#shellcheck shell=bash

notify_current() {
  case "$(niri msg --json keyboard-layouts | jq -r '.names[.current_idx]')" in
    "English"*)     text=US ;;
    "Portuguese"*)  text=PT ;;
  esac

  notify-send \
    --icon "$KEYBOARD_ICON" \
    --category "keyboard-input" \
    --hint string:x-canonical-private-synchronous:keyboard-input \
    --transient \
    "Keyboard Layout: $text"
}

case "${1:-}" in
  next) niri msg action switch-layout next && notify_current  ;;
  get)  notify_current                                        ;;
esac