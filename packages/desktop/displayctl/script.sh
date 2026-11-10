#shellcheck shell=bash

# Switch one screen, or follow the panel to what else is plugged in.
# A screen is named as the Display menu names it: a connector, or make, model and serial.
toggle() {
  outputs=$(niri msg --json outputs)
  conn=$(echo "$outputs" | jq -r --arg want "$1" '
    to_entries[]
    | select(.key == $want or (([.value.make, .value.model, .value.serial] | join(" ")) == $want))
    | .key')
  # Not plugged in, and the menu has already greyed the entry out.
  [ -n "$conn" ] || return 0
  # An output turned off is still listed, only without its `logical` geometry.
  if echo "$outputs" | jq -e --arg conn "$conn" '.[$conn].logical' >/dev/null; then
    niri msg output "$conn" off
    # Nothing lit left is a black screen, so the panel takes over.
    if ! niri msg --json outputs | jq -e "any(.[]; .logical)" >/dev/null; then
      niri msg output "$panel" on || true
    fi
  else
    niri msg output "$conn" on
  fi
}

# Keep the panel dark while any other screen is on: plug something in and the laptop goes
# out, unplug everything and it comes back.
#
# Only the set of *connected* screens is looked at, since switching one off with the menu
# leaves it connected, so this never undoes what the menu just did.
connected=""
panel_follows() {
  outputs=$(niri msg --json outputs)
  now=$(echo "$outputs" | jq -r 'keys | sort | join(" ")')
  [ "$now" != "$connected" ] || return 0
  connected=$now
  if echo "$outputs" | jq -e --arg panel "$panel" 'any(keys[]; . != $panel)' >/dev/null; then
    niri msg output "$panel" off || true
  else
    niri msg output "$panel" on || true
  fi
}

# The kernel says so over udev: niri's IPC has no event for a screen arriving or leaving,
# and a screen switched off is still listed, so a drm change is the only real news here.
follow() {
  panel_follows
  udevadm monitor --subsystem-match=drm | while read -r _; do
    # A screen plugged in this moment has not been picked up by niri yet.
    sleep 0.3
    panel_follows
  done
}

case "${1:-}" in
  toggle) toggle "${2:-}" ;;
  follow) follow ;;
  *)
    echo "usage: displayctl toggle <screen> | follow" >&2
    exit 2
    ;;
esac
