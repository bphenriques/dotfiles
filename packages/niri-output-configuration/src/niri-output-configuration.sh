#shellcheck shell=bash

# TODO: Explore with icons: echo -en 'Bananas\0icon\x1f/nix/store/sch5j89n8dzipmsazl302qa87606f9br-papirus-icon-theme-20240501/share/icons/Papirus/symbolic/devices/video-display-symbolic.svg' | fuzzel --dmenu^

# Functions that depend on the window manager
dmenu() { fuzzel --dmenu --placeholder "Output Configuration" --lines 3; }
notify() { notify-send "Output Configuration" "$1"; }
exists_display() { niri msg --json outputs | jq --exit-status --arg DISPLAY "$1" '.[$DISPLAY]' > /dev/null; }

enable_output() { exists_display "$1" && niri msg output "$1" on; }   # assert first as niri sets the state once connected
disable_output() { exists_display "$1" && niri msg output "$1" off; } # assert first as niri sets the state once connected
enable_single_output() {
  export -f disable_output exists_display
  niri msg --json outputs \
    | jq -r --exit-status --arg DISPLAY "$1" 'keys | .[] | select(. != $DISPLAY)' \
    | xargs -I{} sh -c 'disable_output {}'
  enable_output "$1"
}

# Profile functions
profile_default_set() { profile_external_set || profile_internal_set; }

profile_internal_name() { echo "Internal display"; }
profile_internal_valid() { exists_display eDP-1; }
profile_internal_set() { profile_internal_valid && enable_single_output eDP-1 && profile_internal_name; }

profile_external_name() { echo "External display"; }
profile_external_valid() { exists_display HDMI-A-1; }
profile_external_set() { profile_external_valid && enable_single_output HDMI-A-1 && profile_external_name; }

profile_extend_name() { echo "Extend displays"; }
profile_extend_valid() { exists_display eDP-1 && exists_display HDMI-A-1; }
profile_extend_set() { profile_extend_valid && enable_output eDP-1 && enable_output HDMI-A-1 && profile_extend_name; }

# shellcheck disable=SC2005
profile_list() {
  profile_internal_valid && echo "$(profile_internal_name)"
  profile_external_valid && echo "$(profile_external_name)"
  profile_extend_valid && echo "$(profile_extend_name)"
}

case "${1:-}" in
  dmenu)
    case "$(profile_list | dmenu)" in
      "$(profile_internal_name)") notify "$(profile_internal_set)"  ;;
      "$(profile_external_name)") notify "$(profile_external_set)"  ;;
      "$(profile_extend_name)")   notify "$(profile_extend_set)"    ;;
    esac
    ;;
  default)    notify "$(profile_default_set)"   ;;
  safemode)   notify "$(profile_internal_set)"  ;;
esac