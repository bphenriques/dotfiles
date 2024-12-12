#shellcheck shell=bash

# Functions that depend on the window manager
dmenu() { options | fuzzel --dmenu --placeholder "Output Configuration" --lines 3; }
notify() { notify-send "Output Configuration" "$1"; }
exists_display() { niri msg --json outputs | jq --exit-status --arg DISPLAY "$1" '.[$DISPLAY]' > /dev/null; }
is_display_on() { niri msg --json outputs | jq --exit-status --arg DISPLAY "$1" '.[$DISPLAY] | .current_mode' > /dev/null; }
enable_output() { exists_display "$1" && niri msg output "$1" on; }    # checking first, as niri sends the msg once connected
disable_output() { exists_display "$1" && niri msg output "$1" off; }  # checking first, as niri sends the msg once connected
disable_output_not() {
  export -f disable_output
  niri msg --json outputs \
    | jq -r --exit-status --arg DISPLAY "$1" 'keys | .[] | select(. != $DISPLAY)' \
    | xargs -I{} sh -c 'disable_output {}'
}

# Utility functions
profile_internal_name() { echo "Internal display"; }
profile_internal_valid() { exists_display eDP-1; }
profile_internal_set() { profile_internal_valid && disable_output_not eDP-1 && enable_output eDP-1 && profile_internal_name; }

profile_external_name() { echo "External display"; }
profile_external_valid() { exists_display HDMI-A-1; }
profile_external_set() { profile_external_valid && disable_output_not HDMI-A-1 && enable_output HDMI-A-1 && profile_external_name; }

profile_extend_name() { echo "Extend displays"; }
profile_extend_valid() { exists_display eDP-1 && exists_display HDMI-A-1; }
profile_extend_set() { profile_extend_valid && enable_output eDP-1 && enable_output HDMI-A-1 && profile_extend_name; }

profile_default_set() { profile_external_set || profile_internal_set; }

# shellcheck disable=SC2005
options() {
  profile_internal_valid && echo "$(profile_internal_name)"
  profile_external_valid && echo "$(profile_external_name)"
  profile_extend_valid && echo "$(profile_extend_name)"
}

case "${1:-}" in
  dmenu)
    case "$(dmenu)" in
      "$(profile_internal_name)") notify "$(profile_internal_set)"  ;;
      "$(profile_external_name)") notify "$(profile_external_set)"  ;;
      "$(profile_extend_name)")   notify "$(profile_extend_set)"    ;;
    esac
    ;;
  default)  notify "$(profile_default_set)" ;;
esac
