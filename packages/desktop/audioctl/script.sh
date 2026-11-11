#shellcheck shell=bash

VOLUME_LIMIT=1.5 # wpctl decimal: 1.0 = 100%, 1.5 = 150%

# Device queries (pactl for rich JSON properties, no wpctl equivalent)
list_sources() { pactl -f json list sources | jq -cr '[ .[] | select((.ports // []) | any(.type == "Mic" or .type == "Headset")) ]'; }
list_sinks() { pactl -f json list sinks | jq -cr '[ .[] | select((.ports // []) | length > 0) ]'; }
get_sink() { pactl -f json list sinks | jq -cr --arg name "$1" '.[] | select(.name == $name)'; }
get_source() { pactl -f json list sources | jq -cr --arg name "$1" '.[] | select(.name == $name)'; }

# Wireplumber
set_volume() { wpctl set-volume --limit "$VOLUME_LIMIT" "$1" "$2"; }
set_mute() { wpctl set-mute "$1" "$2"; }

# Classifies a device using pactl JSON: device.form_factor > active port type > device.bus.
# Takes kind (sink/source) to distinguish speakers from microphones.
device_type() {
  local kind="$1"
  echo "$2" | jq -j --arg kind "$kind" '
    . as $root |
    (.properties["device.form_factor"] // "") as $ff |
    (first(.ports[]? | select(.name == $root.active_port) | .type) // "") as $pt |
    (.properties["device.bus"] // "") as $bus |
    if $kind == "source" then
      if   $ff == "headset" or $pt == "Headset" then "headset"
      else                                           "microphone"
      end
    else
      if   $ff == "headset"                          then "headset"
      elif $ff == "headphone"                        then "headphones"
      elif $pt == "Speaker"                          then "internal"
      elif $pt == "Headphones"                       then "headphones"
      elif $pt == "Headset"                          then "headset"
      elif $pt == "HDMI"                             then "external"
      elif $bus == "usb" or $bus == "bluetooth"      then "external"
      else                                                "internal"
      end
    end
  '
}

friendly_device_name() {
  local json="$1" dtype="$2"
  case "$dtype" in
    internal) echo -n "Speakers" ;;
    headphones) echo -n "Headphones" ;;
    *) echo "$json" | jq -j '.properties["node.nick"] // .description' ;;
  esac
}

# Maps device type to the label used in env var names (AUDIOCTL_<label>_ICON)
_device_label() {
  case "$1" in
    microphone) echo -n "MICROPHONE" ;;
    internal) echo -n "INTERNAL_SPEAKERS" ;;
    external) echo -n "EXTERNAL_SPEAKERS" ;;
    headset) echo -n "HEADSET" ;;
    headphones) echo -n "HEADPHONES" ;;
  esac
}

# Returns the icon for a device type through indirect expansion of the AUDIOCTL_*_ICON env vars.
get_icon() {
  local label
  label="$(_device_label "$1")"
  [ -z "$label" ] && return
  local var="AUDIOCTL_${label}_ICON"
  echo -n "${!var}"
}

notify_failure() {
  notify-send \
    --urgency=critical \
    --icon "$AUDIOCTL_ERROR_ICON" \
    --category "audioctl" \
    --hint string:x-canonical-private-synchronous:volume \
    --transient \
    "${1:-Failure while running audioctl}" "${2:-}"
}

# Process substitution (< <(...)) keeps the loop in the main shell so rc accumulates correctly.
move_all_sink_inputs() {
  local target_index="$1" rc=0
  while read -r index; do
    pactl move-sink-input "$index" "$target_index" || rc=1
  done < <(pactl -f json list sink-inputs | jq -r '.[].index')
  return "$rc"
}

move_all_source_outputs() {
  local target_index="$1" rc=0
  while read -r index; do
    pactl move-source-output "$index" "$target_index" || rc=1
  done < <(pactl -f json list source-outputs | jq -r '.[].index')
  return "$rc"
}

# Finds the next/previous device by cycling through indices. Wraps around using jq's modulo and negative array indexing.
get_next_device() {
  local all_devices="$1" current_name="$2" inc="${3:-1}"
  echo "$all_devices" | jq -cr \
    --arg name "$current_name" \
    --arg inc "$inc" '
      [.[].index] as $indices |
      (.[] | select(.name == $name) | .index) as $current |
      ($indices | index($current)) as $pos |
      $indices[(($pos + ($inc | tonumber)) % ($indices | length))] as $next |
      .[] | select(.index == $next) | .name
    '
}

get_next_sink() { get_next_device "$(list_sinks)" "$(pactl get-default-sink)" "${1:-+1}"; }

set_sink_and_move() {
  if ! pactl set-default-sink "$1"; then
    notify_failure "Failed to change sound output"
    return 1
  fi
  if ! move_all_sink_inputs "$(get_sink "$1" | jq -r '.index')"; then
    notify_failure "Failed to move sound output"
  fi
}

set_source_and_move() {
  if ! pactl set-default-source "$1"; then
    notify_failure "Failed to change sound source"
    return 1
  fi
  if ! move_all_source_outputs "$(get_source "$1" | jq -r '.index')"; then
    notify_failure "Failed to move sound source"
  fi
}

# One line per device: rank (internal first), kind, name, type and friendly name.
print_devices() {
  local kind="$1" device dtype rank
  while read -r device; do
    dtype="$(device_type "$kind" "$device")"
    case "$dtype" in internal | microphone) rank=0 ;; *) rank=1 ;; esac
    printf '%s\t%s\t%s\t%s\t%s\n' "$rank" "$kind" "$(jq -r .name <<<"$device")" "$dtype" "$(friendly_device_name "$device" "$dtype")"
  done < <(jq -c '.[]' <<<"$2")
}

device_dmenu_options() {
  local kind="$1" all_devices="$2"
  local device_json dtype
  while IFS= read -r device_json; do
    dtype="$(device_type "$kind" "$device_json")"
    printf '%s\u0000icon\u001f%s\n' \
      "$(friendly_device_name "$device_json" "$dtype")" \
      "$(get_icon "$dtype")"
  done < <(echo "$all_devices" | jq -c '.[]')
}

device_select_fuzzel() {
  local kind="$1" all_devices="$2"
  local current_device prompt

  case "$kind" in
    sink)
      prompt="Output"
      current_device="$(get_sink "$(pactl get-default-sink)")"
      ;;
    source)
      prompt="Input"
      current_device="$(get_source "$(pactl get-default-source)")"
      ;;
  esac

  local current_dtype current_name
  current_dtype="$(device_type "$kind" "$current_device")"
  current_name="$(friendly_device_name "$current_device" "$current_dtype")"

  local names selection
  names="$(echo "$all_devices" | jq -r '[ .[].name ]')"
  selection="$(device_dmenu_options "$kind" "$all_devices" | fuzzel --dmenu --index --width 65 \
    --lines "$(echo "$names" | jq length)" \
    --prompt "$prompt: " \
    --mesg "Current: $current_name")"
  if [[ -n $selection && $selection != -1 ]]; then
    echo "$names" | jq -rc --arg INDEX "$selection" '.[$INDEX | tonumber]'
  fi
}

case "${1:-}" in
  # Sinks (audio output)
  sink-toggle-mute) set_mute @DEFAULT_SINK@ toggle ;;
  sink-increase) set_volume @DEFAULT_SINK@ "${2:-5}%+" ;;
  sink-decrease) set_volume @DEFAULT_SINK@ "${2:-5}%-" ;;
  sink-move-next) set_sink_and_move "$(get_next_sink "+1")" ;;
  sink-move-prev) set_sink_and_move "$(get_next_sink "-1")" ;;
  sink-move-fuzzel)
    selection="$(device_select_fuzzel sink "$(list_sinks)")"
    if [[ -n $selection ]]; then
      set_sink_and_move "$selection"
    fi
    ;;

  # Every output and input, internal first then by name: kind, name, type and friendly name, tab separated (read by the desktop shell).
  devices)
    { print_devices sink "$(list_sinks)"; print_devices source "$(list_sources)"; } | sort -t $'\t' -k2,2 -k1,1n -k5,5 | cut -f2-
    ;;

  select)
    case "$2" in
      sink) set_sink_and_move "$3" ;;
      source) set_source_and_move "$3" ;;
    esac
    ;;

  # Sources (audio input)
  source-toggle-mute) set_mute @DEFAULT_SOURCE@ toggle ;;
  source-increase) set_volume @DEFAULT_SOURCE@ "${2:-5}%+" ;;
  source-decrease) set_volume @DEFAULT_SOURCE@ "${2:-5}%-" ;;
  source-move-fuzzel)
    selection="$(device_select_fuzzel source "$(list_sources)")"
    if [[ -n $selection ]]; then
      set_source_and_move "$selection"
    fi
    ;;
esac
