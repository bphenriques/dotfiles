#shellcheck shell=bash
exists_cmd() { command -v "$1" >/dev/null; }
open_dmenu() {
  if exists_cmd fuzzel; then
    fuzzel --dmenu
  else
    echo "No compatible dmenu runner found" >&2;
    exit 1
  fi
}

options() {
  echo "Lock"
  echo "Logout"
  echo "Reboot"
  echo "Shutdown"
  echo "Suspend"
}

case "$(options | open_dmenu)" in
  "Lock")       blurlock                            ;;
  "Logout")     loginctl terminate-user "$(whoami)" ;;
  "Reboot")     systemctl reboot                    ;;
  "Shutdown")   systemctl poweroff                  ;;
  "Suspend")    systemctl suspend                   ;;
esac