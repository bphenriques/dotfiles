#shellcheck shell=bash

get_sink()                { pactl -f json list sinks | jq -cr --arg sink "$1" '.[] | select(.name == $sink)'; }
is_default_sink_muted()   { pactl get-sink-mute @DEFAULT_SINK@ | grep -q "Mute: yes"; }
get_default_sink_volume() { pactl get-sink-volume @DEFAULT_SINK@ | grep -Po '\d+(?=%)' | head -n 1; }

device_type() {
  case "$1" in
    *SteelSeries*)  echo -n "headset"      ;;
    *analog*)       echo -n "internal"     ;;
    *)              echo -n "external"     ;;
  esac
}

friendly_sink_name() {
  local sink_name
  sink_name="$(get_sink "$1" | jq -r '.name')"
  case "${sink_name}" in
    *analog*) echo -n "Internal Speakers"           ;;
    *hdmi*)   echo -n "HDMI Output"                 ;;
    *)        get_sink "$1" | jq -r '.description'  ;;
  esac
}

get_icon() {
  local device_type="$1"
  case "$device_type" in
    internal)     echo -n "${OSD_VOLUME_INTERNAL_SPEAKERS_ICON}" ;;
    external)     echo -n "${OSD_VOLUME_EXTERNAL_SPEAKERS_ICON}" ;;
    headset)      echo -n "${OSD_VOLUME_HEADSET_ICON}" ;;
    headphones)   echo -n "${OSD_VOLUME_HEADPHONES_ICON}" ;;
  esac
}

get_mute_icon() {
  local device_type="$1"
  case "$device_type" in
    internal)     echo -n "${OSD_VOLUME_INTERNAL_SPEAKERS_MUTE_ICON}" ;;
    external)     echo -n "${OSD_VOLUME_EXTERNAL_SPEAKERS_MUTE_ICON}" ;;
    headset)      echo -n "${OSD_VOLUME_HEADSET_MUTE_ICON}" ;;
    headphones)   echo -n "${OSD_VOLUME_HEADPHONES_MUTE_ICON}" ;;
  esac
}

notify_current_sink_volume() {
  current="$(pactl get-default-sink)"
  progress="$(get_default_sink_volume)"
  icon=
  if is_default_sink_muted || [ "$progress" -eq 0 ]; then
    icon="$(get_mute_icon "$(device_type "$current")")"
    progress=0
  else
    icon="$(get_icon "$(device_type "$current")")"
  fi

  notify-send \
    --expire-time 1500 \
    --icon "$icon" \
    --category "volume-osd" \
    --hint string:x-canonical-private-synchronous:volume \
    --hint string:x-dunst-stack-tag:volume \
    --hint int:value:"$progress" \
    --transient \
    "Volume: $progress%"
}

notify_current_sink() {
  current="$(pactl get-default-sink)"
  progress="$(get_default_sink_volume)"
  icon="$(get_icon "$(device_type "$current")")"
  notify-send \
    --expire-time 3000 \
    --icon "$icon" \
    --category "volume-osd" \
    --hint string:x-canonical-private-synchronous:volume \
    --hint string:x-dunst-stack-tag:volume \
    --hint int:value:"$progress" \
    --transient \
    "$(friendly_sink_name "$(pactl get-default-sink)")"
}

notify_failure() {
  notify-send \
    --urgency=critical \
    --expire-time 3000 \
    --icon "$OSD_VOLUME_ERROR_ICON" \
    --category "volume-osd" \
    --hint string:x-canonical-private-synchronous:volume \
    --hint string:x-dunst-stack-tag:volume \
    --transient \
    "${1:-'Failure while running volume-osd'}" "${2:-}"
}

move_all_inputs() {
  target_sink_index="$1"
  for sink_input_index in $(pactl -f json list sink-inputs | jq -r '.[].index'); do
    pactl move-sink-input "$sink_input_index" "$target_sink_index"
  done
}

get_next_sink() {
  inc="${1:-"+1"}"
  current="$(get_sink "$(pactl get-default-sink)" | jq -cr '.index')"
  current_pos_index="$(pactl -f json list sinks | jq -cr --arg current "${current}" '[.[].index] | index($current | tonumber)')"
  number_sinks="$(pactl -f json list sinks | jq 'length')"
  next_index="$(pactl -f json list sinks | jq -cr --arg inc "$inc" --arg current "$current_pos_index" --arg total "$number_sinks" '[.[].index][(($current | tonumber)+($inc | tonumber)) % ($total | tonumber)]')"

  pactl -f json list sinks | jq -cr --arg index "$next_index" '.[] | select(.index == ($index | tonumber)) | .name'
}

set_sink_and_move() {
  if ! pactl set-default-sink "$1"; then
    notify_failure "Failed to change sound output"
    return 1
  fi

  if ! move_all_inputs "$(get_sink "$1" | jq -r '.index')"; then
    notify_failure "Failed to move sound output"
  fi
}

sink_dmenu_options() {
  for sink_name in $(pactl -f json list sinks | jq -r '.[] | .name'); do
    printf '%s\u0000icon\u001f%s\n' "$(friendly_sink_name "$sink_name")" "$(get_icon "$(device_type "$sink_name")")"
  done
}

sink_select_fuzzel() {
  sinks="$(pactl -f json list sinks | jq -r '[ .[] | .name ]')"
  selection="$(sink_dmenu_options | fuzzel --dmenu --index --width 65 --lines "$(echo "$sinks" | jq length)")"
  if [ "$selection" != -1 ]; then
    echo "$sinks" | jq -rc --arg INDEX "$selection" '.[$INDEX | tonumber]'
  fi
}

case "${1:-}" in
  sink-toggle-mute) pactl set-sink-mute @DEFAULT_SINK@ toggle && notify_current_sink_volume                     ;;
  sink-increase)    shift 1 && pactl set-sink-volume @DEFAULT_SINK@ "+${1:-5}%" && notify_current_sink_volume   ;;
  sink-decrease)    shift 1 && pactl set-sink-volume @DEFAULT_SINK@ "-${1:-5}%" && notify_current_sink_volume   ;;
  sink-move)        shift 1 && set_sink_and_move "$1" && notify_current_sink                                    ;;
  sink-move-next)   set_sink_and_move "$(get_next_sink "+1")" && notify_current_sink                            ;;
  sink-move-prev)   set_sink_and_move "$(get_next_sink "-1")" && notify_current_sink                            ;;
  sink-move-fuzzel)
    selection="$(sink_select_fuzzel)"
    if [ "$selection" != "" ]; then
      set_sink_and_move "$selection" && notify_current_sink
    fi
    ;;
esac