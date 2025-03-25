#shellcheck shell=bash

notify() { notify-send --category "screenshot" --hint string:x-canonical-private-synchronous:screenshot "Screenshot" "$1"; }

case "${1:-}" in
  screen)
    shift 1
    destination="$1/screenshot-$(date +'%Y%m%d-%H%M%S').png"
    grim "$destination"
    notify "$destination"
    ;;
  region)
    shift 1
    destination="$1/screenshot-$(date +'%Y%m%d-%H%M%S').png"
    grim -g "$(slurp)" "$destination"
    notify "$destination"
    ;;
  screen-copy)  grim - | wl-copy                     ;;
  screen-edit)  grim - | swappy -f -                 ;;
  region-copy)  grim -g "$(slurp)" - | wl-copy       ;;
  region-edit)  grim -g "$(slurp)" - | swappy -f -   ;;
esac