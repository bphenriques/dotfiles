#shellcheck shell=bash
set_wallpaper() { test -f "$1" && swww img --transition-type none "$1"; }
random_file() { find "$1" -type f | sort -R | head -1 ; }

case "${1:-}" in
  random) shift 1 && set_wallpaper "$(random_file "$1")"  ;;
  one)    shift 1 && set_wallpaper "$1"                   ;;
esac
