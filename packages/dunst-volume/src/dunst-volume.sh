#shellcheck shell=sh

# From Adwaita
MUTED_ICON="${DUNST_VOLUME_MUTED_ICON:-audio-volume-muted-symbolic}"
LOW_ICON="${DUNST_VOLUME_LOW_ICON:-audio-volume-low-symbolic}"
MEDIUM_ICON="${DUNST_VOLUME_MEDIUM_ICON:-audio-volume-medium-symbolic}"
HIGH_ICON="${DUNST_VOLUME_HIGH_ICON:-audio-volume-high-symbolic}"



#        XF86AudioRaiseVolume allow-when-locked=true { spawn "wpctl" "set-volume" "@DEFAULT_AUDIO_SINK@" "0.1+"; }
#        XF86AudioLowerVolume allow-when-locked=true { spawn "wpctl" "set-volume" "@DEFAULT_AUDIO_SINK@" "0.1-"; }
#        XF86AudioMute        allow-when-locked=true { spawn "wpctl" "set-mute" "@DEFAULT_AUDIO_SINK@" "toggle"; }
#        XF86AudioMicMute     allow-when-locked=true { spawn "wpctl" "set-mute" "@DEFAULT_AUDIO_SOURCE@" "toggle"; }


get_percentage() { ponymix get-volume; }
is_muted() { ponymix is-muted; }
increase() { ponymix increase "$1"; }
decrease() { ponymix decrease "$1"; }
mute() { ponymix mute; }
unmute() { ponymix unmute; }
toggle_mute() { ponymix toggle; }

notify() {
  percentage="$(get_percentage)"
  icon=
  if is_muted || [ "$percentage" -eq 0 ]; then
    icon="$MUTED_ICON"
    progress=0
  elif [ "$percentage" -lt 30 ]; then
    icon="$LOW_ICON"
    progress="$percentage"
  elif [ "$percentage" -lt 70 ]; then
    icon="$MEDIUM_ICON"
    progress="$percentage"
  else
    icon="$HIGH_ICON"
    progress="$percentage"
  fi

  dunstify \
    --timeout 1500 \
    --appname "volume-osd" \
    --hints string:x-canonical-private-synchronous:volume \
    --hints string:x-dunst-stack-tag:volume \
    --hints int:value:"$progress" \
    --icon "$icon" \
    "Volume: $progress%"
}

case "${1:-}" in
  mute)         mute && notify                            ;;
  unmute)       unmute && notify                          ;;
  toggle-mute)  toggle_mute && notify                     ;;
  increase)     shift 1 && increase "${1:-10}" && notify  ;;
  decrease)     shift 1 && decrease "${1:-10}" && notify  ;;
  switch) ;;
esac