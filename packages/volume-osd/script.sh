#shellcheck shell=bash

is_sink_muted()   { pactl get-sink-mute "$1" | grep -q "Mute: yes"; }
get_sink()        { pactl -f json list | jq -cr --arg sink "$1" '.sinks[] | select(.name == $sink)'; }
get_sink_volume() { pactl get-sink-volume @DEFAULT_SINK@ | grep -Po '\d+(?=%)' | head -n 1; }

device_type() {
  case "$(pactl get-default-sink)" in
    *SteelSeries*)  echo -n "headset"      ;;
    *analog*)       echo -n "internal"     ;;
    *)              echo -n "external"     ;;
  esac
}

friendly_sink_name() {
  sink_name="$(get_sink "$(pactl get-default-sink)" | jq -r '.name')"
  case "${sink_name}" in
    *analog*) echo -n "Internal Speakers"                  ;;
    *hdmi*)   echo -n "HDMI Output"                        ;;
    *)        get_sink "$(pactl get-default-sink)" | jq -cr '.description' ;;
  esac
}

notify_volume() {
  progress="$(get_sink_volume)"
  icon=
  icon_muted=
  case "$(device_type)" in
    internal)
      icon="${OSD_VOLUME_INTERNAL_SPEAKERS_ICON}"
      icon_muted="${OSD_VOLUME_INTERNAL_SPEAKERS_MUTE_ICON}"
      ;;
    external)
      icon="${OSD_VOLUME_EXTERNAL_SPEAKERS_ICON}"
      icon_muted="${OSD_VOLUME_EXTERNAL_SPEAKERS_MUTE_ICON}"
      ;;
    headset)
      icon="${OSD_VOLUME_HEADSET_ICON}"
      icon_muted="${OSD_VOLUME_HEADSET_MUTE_ICON}"
      ;;
    headphones)
      icon="${OSD_VOLUME_HEADPHONES_ICON}"
      icon_muted="${OSD_VOLUME_HEADPHONES_MUTE_ICON}"
      ;;
  esac

  if is_sink_muted @DEFAULT_SINK@ || [ "$progress" -eq 0 ]; then
    icon="$icon_muted"
    progress=0
  fi

  notify-send \
    --expire-time 1500 \
    --icon "$icon" \
    --category "volume-osd" \
    --hint string:x-canonical-private-synchronous:volume \
    --hint string:x-dunst-stack-tag:volume \
    --hint int:value:"$progress" \
    "Volume: $progress%"
}

notify_sink() {
  percentage="$(get_sink_volume)"
  icon=
  case "$(device_type)" in
    internal)   icon="${OSD_VOLUME_INTERNAL_SPEAKERS_ICON}" ;;
    external)   icon="${OSD_VOLUME_EXTERNAL_SPEAKERS_ICON}" ;;
    headset)    icon="${OSD_VOLUME_HEADSET_ICON}"           ;;
    headphones) icon="${OSD_VOLUME_HEADPHONES_ICON}"        ;;
  esac

  progress="$percentage"
  notify-send \
    --expire-time 1500 \
    --icon "$icon" \
    --category "volume-osd" \
    --hint string:x-canonical-private-synchronous:volume \
    --hint string:x-dunst-stack-tag:volume \
    --hint int:value:"$progress" \
    "$(friendly_sink_name)"
}

move_all_inputs() {
  target_sink_index="$1"
  for sink_input_index in $(pactl -f json list sink-inputs | jq -r '.[].index'); do
    pactl move-sink-input "$sink_input_index" "$target_sink_index"
  done
}

get_next_sink() {
  current="$(get_sink "$(pactl get-default-sink)" | jq -cr '.index')"
  current_pos_index="$(pactl -f json list sinks | jq -cr --arg current "${current}" '[.[].index] | index($current | tonumber)')"
  number_sinks="$(pactl -f json list sinks | jq 'length')"
  next_index="$(pactl -f json list sinks | jq -cr --arg current "$current_pos_index" --arg total "$number_sinks" '[.[].index][(($current | tonumber)+1) % ($total | tonumber)]')"

  pactl -f json list sinks | jq -cr --arg index "$next_index" '.[] | select(.index == ($index | tonumber))'
}

case "${1:-}" in
  sink-toggle-mute)       pactl set-sink-mute @DEFAULT_SINK@ toggle && notify_volume                    ;;
  sink-increase)          shift 1 && pactl set-sink-volume @DEFAULT_SINK@ "+${1:-5}%" && notify_volume  ;;
  sink-decrease)          shift 1 && pactl set-sink-volume @DEFAULT_SINK@ "-${1:-5}%" && notify_volume  ;;
  sink-move)
    shift 1
    pactl set-default-sink "$1"
    current_index="$(get_sink "$(pactl get-default-sink)" | jq -r '.name')"
    move_all_inputs "$(echo "$current_index" | jq -cr '.index')"
    notify_sink
    ;;
  sink-move-next)
    shift 1
    next="$(get_next_sink)"
    pactl set-default-sink "$(echo "$next" | jq -cr '.name')"
    move_all_inputs "$(echo "$next" | jq -cr '.index')"
    notify_sink
    ;;
esac