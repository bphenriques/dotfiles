#shellcheck shell=bash

# --- Battery (via upower-notify) ---
battery_line="" battery_severity=normal
if IFS=$'\t' read -r bat_state bat_pct bat_glyph bat_severity < <(upower-notify status 2>/dev/null); then
  battery_severity="$bat_severity"
  if [ "$bat_state" = "charging" ]; then
    battery_line="${bat_glyph}  ${bat_pct}% (charging)"
  else
    battery_line="${bat_glyph}  ${bat_pct}%"
  fi
fi

# --- Network ---
network_line="󰖪  Disconnected"
active="$(nmcli -t -f TYPE,NAME connection show --active 2>/dev/null || true)"
active="${active%%$'\n'*}"
if [ -n "$active" ]; then
  net_type="${active%%:*}"
  net_name="${active#*:}"
  case "$net_type" in
    *wireless*|*wifi*)   network_line="󰖩  ${net_name}" ;;
    *ethernet*)          network_line="󰈀  Ethernet" ;;
    *vpn*|*wireguard*)   network_line="󰌾  VPN (${net_name})" ;;
    *)                   network_line="󰛳  ${net_name}" ;;
  esac
fi

# --- Audio output (via volume-osd) ---
audio_line=""
if IFS=$'\t' read -r _audio_type audio_name audio_vol audio_muted audio_glyph < <(volume-osd sink-status 2>/dev/null); then
  if [ "$audio_muted" = "true" ]; then
    audio_line="${audio_glyph}  ${audio_name} (muted)"
  else
    audio_line="${audio_glyph}  ${audio_name} ${audio_vol}%"
  fi
fi

# --- Keyboard layout (via niri-keyboard-layout) ---
layout_line=""
if layout="$(niri-keyboard-layout status 2>/dev/null)"; then
  [ -n "$layout" ] && layout_line="󰌌  ${layout}"
fi

# --- Build notification ---
time="$(date +'%a, %d %b  %H:%M')"

lines=()
[ -n "$battery_line" ] && lines+=("$battery_line")
[ -n "$network_line" ] && lines+=("$network_line")
[ -n "$audio_line" ]   && lines+=("$audio_line")
[ -n "$layout_line" ]  && lines+=("$layout_line")

body="$(printf '%s\n' "${lines[@]}")"
body="${body%$'\n'}"

notify-send \
  --urgency="$battery_severity" \
  --category "status-glance" \
  --hint string:x-canonical-private-synchronous:status-glance \
  --expire-time 5000 \
  --transient \
  "$time" "$body"
