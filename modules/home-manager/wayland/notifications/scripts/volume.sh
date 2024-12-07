#!/bin/sh

# TODO: See https://github.com/dunst-project/dunst/blob/master/contrib/progress-notify.sh
# Similar thing for brigthness? cat /sys/class/backlight/amdgpu_bl1/

get_volume() { amixer get Master | grep '%' | head -n 1 | cut -d '[' -f 2 | cut -d '%' -f 1; }
is_muted() { amixer get Master | grep '%' | grep -oE '[^ ]+$' | grep off > /dev/null; }
send_notification() {
  dunstify -h string:x-canonical-private-synchronous:audio \
    --icon H \
    "Volume: " -h int:value:"$(get_volume)";
}

case $1 in
  up)
    amixer -q sset Master 5%+ --quiet
    send_notification
  ;;
  down)
    amixer -q sset Master 5%- --quiet
    send_notification
  	;;
  mute)
    amixer -q sset Master toggle
    amixer -D pulse set Master 1+ toggle > /dev/null
    if is_muted ; then
      dunstify -h string:x-canonical-private-synchronous:audio "Volume: 0"
    else
      send_notification
    fi
	;;
esac