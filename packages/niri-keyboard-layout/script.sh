#shellcheck shell=bash

notify_current() {
  case "$(niri msg --json keyboard-layouts | jq -r '.names[.current_idx]')" in
    "English"*)     text=US ;;
    "Portuguese"*)  text=PT ;;
  esac

  notify-send \
    --expire-time 1500 \
    --icon "$KEYBOARD_ICON" \
    --category "keyboard-input" \
    --hint string:x-canonical-private-synchronous:keyboard-input \
    --hint string:x-dunst-stack-tag:keyboard-input \
    "Keyboard Layout: $text"
}

case "${1:-}" in
  next) niri msg action switch-layout next && notify_current  ;;
  get)  notify_current                                        ;;
esac