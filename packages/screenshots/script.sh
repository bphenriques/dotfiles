#shellcheck shell=bash

notify() {
  tempnotify='notify-send -u low --action=edit=Edit --action=save=Save'
  wl-paste > $dir/$file
  temp=$(${tempnotify} -i $dir/$file "Copied to clipboard. ")
  rm $dir/$file
  if [ $temp = 'edit' ]; then
    wl-paste > $dir/$file
    swappy -f $dir/$file
    rm $dir/$file
  elif [ $temp = 'open' ]; then
    wl-paste > $dir/$file
    $filemanager $dir
  fi
}

case "${1:-}" in
  screen)       shift 1 && grim "$1"                            ;;
  screen-copy)  shift 1 && grim - | wl-copy                     ;;
  screen-edit)  shift 1 && grim - | swappy -f -                 ;;
  region)       shift 1 && grim -g "$(slurp)" "$1"              ;;
  region-copy)  shift 1 && grim -g "$(slurp)" - | wl-copy       ;;
  region-edit)  shift 1 && grim -g "$(slurp)" - | swappy -f -   ;;
  *)            echo "Unknown command" && exit 1                ;;
esac