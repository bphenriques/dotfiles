#shellcheck shell=bash

# TODO Migrate more calls to pipewire/wireplumber: https://gitlab.freedesktop.org/pipewire/pipewire/-/wikis/Migrate-PulseAudio#set-card-profile
list_sources()    { pactl -f json list sources | jq -cr '[ .[] | select((.ports | any((.type == "Mic") or (.type == "Headset")))) ]'; }
list_sinks()      { pactl -f json list sinks; }
get_sink()        { pactl -f json list sinks | jq -cr --arg sink "$1" '.[] | select(.name == $sink)'; }
get_source()      { pactl -f json list sources | jq -cr --arg source "$1" '.[] | select(.name == $source)'; }
is_sink_muted()   { pactl get-sink-mute "$1" | grep -q "Mute: yes"; }
is_source_muted() { pactl get-source-mute "$1" | grep -q "Mute: yes"; }

# Wireplumber
get_volume()  { wpctl get-volume "$1" | grep -Po '[\d].[\d]+' | awk '{ print $1 * 100 }'; }
set_volume()  { wpctl set-volume "$1" "$2"; }
set_mute()    { wpctl set-mute "$1" "$2"; }

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
    *analog*) echo -n "Laptop"                      ;;
    *hdmi*)   echo -n "HDMI"                        ;;
    *)        get_sink "$1" | jq -r '.description'  ;;
  esac
}

friendly_source_name() {
  local source_name
  source_name="$(get_source "$1" | jq -r '.name')"
  case "${source_name}" in
    *analog*) echo -n "Laptop"                        ;;
    *)        get_source "$1" | jq -r '.description'  ;;
  esac
}

get_sink_icon() {
  local device_type="$1"
  case "$device_type" in
    internal)     echo -n "${OSD_VOLUME_INTERNAL_SPEAKERS_ICON}" ;;
    external)     echo -n "${OSD_VOLUME_EXTERNAL_SPEAKERS_ICON}" ;;
    headset)      echo -n "${OSD_VOLUME_HEADSET_ICON}" ;;
    headphones)   echo -n "${OSD_VOLUME_HEADPHONES_ICON}" ;;
  esac
}

get_sink_mute_icon() {
  local device_type="$1"
  case "$device_type" in
    internal)     echo -n "${OSD_VOLUME_INTERNAL_SPEAKERS_MUTE_ICON}" ;;
    external)     echo -n "${OSD_VOLUME_EXTERNAL_SPEAKERS_MUTE_ICON}" ;;
    headset)      echo -n "${OSD_VOLUME_HEADSET_MUTE_ICON}" ;;
    headphones)   echo -n "${OSD_VOLUME_HEADPHONES_MUTE_ICON}" ;;
  esac
}

get_source_icon() {
  local device_type="$1"
  case "$device_type" in
    internal)     echo -n "${OSD_VOLUME_MICROPHONE_ICON}" ;;
    headset)      echo -n "${OSD_VOLUME_HEADSET_ICON}" ;;
  esac
}

get_source_mute_icon() {
  local device_type="$1"
  case "$device_type" in
    internal)     echo -n "$OSD_VOLUME_MICROPHONE_MUTE_ICON" ;;
    headset)      echo -n "$OSD_VOLUME_HEADSET_MUTE_ICON" ;;
  esac
}

notify_current_sink() {
  current="$(pactl get-default-sink)"
  progress="$(get_volume @DEFAULT_SINK@)"
  icon=
  if is_sink_muted @DEFAULT_SINK@ || [ "$progress" -eq 0 ]; then
    icon="$(get_sink_mute_icon "$(device_type "$current")")"
    progress=0
  else
    icon="$(get_sink_icon "$(device_type "$current")")"
  fi

  notify-send \
    --icon "$icon" \
    --category "volume-osd" \
    --hint string:x-canonical-private-synchronous:volume \
    --hint int:value:"$progress" \
    --transient \
    "$(friendly_sink_name "$(pactl get-default-sink)")"
}

notify_current_source() {
  current="$(pactl get-default-source)"
  progress="$(get_volume @DEFAULT_SOURCE@)"
  icon=
  if is_source_muted @DEFAULT_SOURCE@ || [ "$progress" -eq 0 ]; then
    icon="$(get_source_mute_icon "$(device_type "$current")")"
    progress=0
  else
    icon="$(get_source_icon "$(device_type "$current")")"
  fi

  notify-send \
    --icon "$icon" \
    --category "volume-osd" \
    --hint string:x-canonical-private-synchronous:volume \
    --hint int:value:"$progress" \
    --transient \
    "$(friendly_source_name "$(pactl get-default-source)")"
}

notify_failure() {
  notify-send \
    --urgency=critical \
    --icon "$OSD_VOLUME_ERROR_ICON" \
    --category "volume-osd" \
    --hint string:x-canonical-private-synchronous:volume \
    --transient \
    "${1:-'Failure while running volume-osd'}" "${2:-}"
}

move_all_sink_inputs() {
  target_sink_index="$1"
  for sink_input_index in $(pactl -f json list sink-inputs | jq -r '.[].index'); do
    pactl move-sink-input "$sink_input_index" "$target_sink_index"
  done
}

get_next_sink() {
  inc="${1:-"+1"}"
  current="$(get_sink "$(pactl get-default-sink)" | jq -cr '.index')"
  current_pos_index="$(list_sinks | jq -cr --arg current "${current}" '[.[].index] | index($current | tonumber)')"
  number_sinks="$(list_sinks | jq 'length')"
  next_index="$(list_sinks | jq -cr --arg inc "$inc" --arg current "$current_pos_index" --arg total "$number_sinks" '[.[].index][(($current | tonumber)+($inc | tonumber)) % ($total | tonumber)]')"

  list_sinks | jq -cr --arg index "$next_index" '.[] | select(.index == ($index | tonumber)) | .name'
}

set_sink_and_move() {
  if ! pactl set-default-sink "$1"; then
    notify_failure "Failed to change sound output"
    return 1
  fi

  if ! move_all_sink_inputs "$(get_sink "$1" | jq -r '.index')"; then
    notify_failure "Failed to move sound output"
  fi
}

sink_dmenu_options() {
  for sink_name in $(list_sinks | jq -r '.[].name'); do
    printf '%s\u0000icon\u001f%s\n' "$(friendly_sink_name "$sink_name")" "$(get_sink_icon "$(device_type "$sink_name")")"
  done
}

sink_select_fuzzel() {
  sinks="$(list_sinks | jq -r '[ .[].name ]')"
  selection="$(sink_dmenu_options | fuzzel --dmenu --index --width 65 --lines "$(echo "$sinks" | jq length)")"
  if [ "$selection" != -1 ]; then
    echo "$sinks" | jq -rc --arg INDEX "$selection" '.[$INDEX | tonumber]'
  fi
}

move_all_source_outputs() {
  target_source_index="$1"
  for index in $(pactl -f json list  source-outputs | jq -r '.[].index'); do
    pactl move-sink-input "$index" "$target_source_index"
  done
}

get_next_source() {
  inc="${1:-"+1"}"
  current="$(get_source "$(pactl get-default-source)" | jq -cr '.index')"
  current_pos_index="$(list_sources | jq -cr --arg current "${current}" '[.[].index] | index($current | tonumber)')"
  number_sources="$(list_sources | jq 'length')"
  next_index="$(list_sources | jq -cr --arg inc "$inc" --arg current "$current_pos_index" --arg total "$number_sources" '[.[].index][(($current | tonumber)+($inc | tonumber)) % ($total | tonumber)]')"

  list_sources | jq -cr --arg index "$next_index" '.[] | select(.index == ($index | tonumber)) | .name'
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

source_dmenu_options() {
  for name in $(list_sources | jq -r '.[] | .name'); do
    printf '%s\u0000icon\u001f%s\n' "$(friendly_source_name "$name")" "$(get_source_icon "$(device_type "$name")")"
  done
}

source_select_fuzzel() {
  sources="$(list_sources | jq -r '[ .[] | .name ]')"
  selection="$(source_dmenu_options | fuzzel --dmenu --index --width 65 --lines "$(echo "$sources" | jq length)")"
  if [ "$selection" != -1 ]; then
    echo "$sources" | jq -rc --arg INDEX "$selection" '.[$INDEX | tonumber]'
  fi
}

case "${1:-}" in
  # For sinks
  sink-toggle-mute)   set_mute @DEFAULT_SINK@ toggle && notify_current_sink                 ;;
  sink-increase)      set_volume @DEFAULT_SINK@ "${2:-5}%+" && notify_current_sink          ;;
  sink-decrease)      set_volume @DEFAULT_SINK@ "${2:-5}%-" && notify_current_sink          ;;
  sink-move)          set_sink_and_move "$2" && notify_current_sink                         ;;
  sink-move-next)     set_sink_and_move "$(get_next_sink "+1")" && notify_current_sink      ;;
  sink-move-prev)     set_sink_and_move "$(get_next_sink "-1")" && notify_current_sink      ;;
  sink-move-fuzzel)
    selection="$(sink_select_fuzzel)"
    if [ "$selection" != "" ]; then
      set_sink_and_move "$selection" && notify_current_sink
    fi
    ;;

  # For sources
  source-toggle-mute) set_mute @DEFAULT_SOURCE@ toggle && notify_current_source              ;;
  source-increase)    set_volume @DEFAULT_SOURCE@ "${2:-5}%+" && notify_current_source       ;;
  source-decrease)    set_volume @DEFAULT_SOURCE@ "${2:-5}%-" && notify_current_source       ;;
  source-move)        set_source_and_move "$2" && notify_current_source                      ;;
  source-move-next)   set_source_and_move "$(get_next_source "+1")" && notify_current_source ;;
  source-move-prev)   set_source_and_move "$(get_next_source "-1")" && notify_current_source ;;
  source-move-fuzzel)
    selection="$(source_select_fuzzel)"
    if [ "$selection" != "" ]; then
      set_source_and_move "$selection" && notify_current_source
    fi
    ;;
esac
