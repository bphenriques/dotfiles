#shellcheck shell=bash
shutdown="    Shutdown"
reboot="    Reboot"
lock="    Lock"
suspend="󰤄    Suspend"
windows="    Reboot to Windows"
system_monitor="󰞱    System Monitor"

chosen="$(echo -e "$lock\n$suspend\n$reboot\n$windows\n$shutdown\n$system_monitor" | fuzzel --dmenu --width 15 --lines 6)"
case ${chosen} in
  "$shutdown")        systemctl poweroff                                                ;;
  "$reboot")          systemctl reboot                                                  ;;
  "$lock")            niri msg action do-screen-transition --delay-ms 500 && hyprlock   ;;
  "$windows")         reboot-to-windows                                                 ;;
  "$suspend")         systemctl suspend                                                 ;;
  "$system_monitor")  footclient --title=btop-tui btop                                  ;;
esac
