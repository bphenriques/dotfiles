#shellcheck shell=bash

case "${1:-}" in
  screen)       shift 1 && grim "$1"                            ;;
  screen-copy)  shift 1 && grim - | wl-copy                     ;;
  screen-edit)  shift 1 && grim - | swappy -f -                 ;;
  region)       shift 1 && grim -g "$(slurp)" "$1"              ;;
  region-copy)  shift 1 && grim -g "$(slurp)" - | wl-copy       ;;
  region-edit)  shift 1 && grim -g "$(slurp)" - | swappy -f -   ;;
  *)            echo "Unknown command" && exit 1                ;;
esac