#shellcheck shell=bash

#notify() {
#  tempnotify='notify-send -u low --action=edit=Edit --action=save=Save'
#  wl-paste > $dir/$file
#  temp=$(${tempnotify} -i $dir/$file "Copied to clipboard. ")
#  rm $dir/$file
#  if [ $temp = 'edit' ]; then
#    wl-paste > $dir/$file
#    swappy -f $dir/$file
#    rm $dir/$file
#  elif [ $temp = 'open' ]; then
#    wl-paste > $dir/$file
#    $filemanager $dir
#  fi
#}

notify() {
  message="$1"
  notify-send \
    --expire-time 3000 \
    --category "screenshot" \
    --hint string:x-canonical-private-synchronous:screenshot \
    --hint string:x-dunst-stack-tag:screenshot \
    "Screenshot" "$message"
}

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