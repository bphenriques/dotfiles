#shellcheck shell=bash

# Screens are niri connectors here; the panel is the laptop's, every other one is an external.
state="${XDG_STATE_HOME:-$HOME/.local/state}/displayctl/layouts.json"

outputs() { niri msg --json outputs; }
externals() { jq -r --arg panel "$panel" 'keys[] | select(. != $panel)' <<<"$1"; }
# The externals by make, model and serial, so a screen is the same on any port.
setup() { jq -r --arg panel "$panel" '[to_entries[] | select(.key != $panel) | .value | "\(.make) \(.model) \(.serial // "Unknown")"] | sort | join(" + ")' <<<"$1"; }
# A screen unplugged a moment ago is gone, and that is fine.
switch() { niri msg output "$1" "$2" || true; }

# The layout the screens are in, empty if none fits. An output that is off has no `logical` geometry.
current() {
  jq -r --arg panel "$panel" '
    (.[$panel].logical != null) as $laptop |
    (to_entries | map(select(.key != $panel)) | any(.value.logical != null)) as $external |
    if $laptop and $external then "extend" elif $external then "external" elif $laptop then "laptop" else "" end' <<<"$1"
}

# The layout last picked with these externals plugged in, External only the first time.
wanted() {
  local setup
  setup=$(setup "$1")
  if [ -z "$setup" ]; then echo laptop; return; fi
  jq -r --arg setup "$setup" '.[$setup] // "external"' "$state" 2>/dev/null || echo external
}

remember() {
  mkdir -p "${state%/*}"
  { cat "$state" 2>/dev/null || echo '{}'; } | jq --arg setup "$1" --arg layout "$2" '.[$setup] = $layout' >"$state.new"
  mv "$state.new" "$state"
}

# laptop: the panel alone; external: every external, panel off; extend: every screen.
# What is needed is turned on before anything is turned off, so a screen stays lit.
layout() {
  local all setup ext
  all=$(outputs)
  mapfile -t ext < <(externals "$all")
  case "$1" in
    laptop)
      switch "$panel" on
      for o in "${ext[@]}"; do switch "$o" off; done
      ;;
    external)
      [ "${#ext[@]}" -gt 0 ] || return 0
      for o in "${ext[@]}"; do switch "$o" on; done
      switch "$panel" off
      ;;
    extend)
      for o in "${ext[@]}"; do switch "$o" on; done
      switch "$panel" on
      ;;
    *) usage ;;
  esac
  setup=$(setup "$all")
  [ -z "$setup" ] || remember "$setup" "$1"
}

# The layout the screens are in, then the layouts the connected screens allow, one per line.
status() {
  local all
  all=$(outputs)
  current "$all"
  echo laptop
  [ -z "$(externals "$all")" ] || printf 'external\nextend\n'
}

# Whenever the set of connected screens changes, the layout wanted for it, unless already in it:
# starting again changes nothing, and switching a screen by hand leaves the set as it is.
# niri reports no screen arriving or leaving, but its workspaces move with them.
follow() {
  local connected="" all now want event
  while read -r event; do
    [[ $event == '{"WorkspacesChanged"'* ]] || continue
    all=$(outputs)
    now=$(jq -r 'keys | sort | join(" ")' <<<"$all")
    [ "$now" != "$connected" ] || continue
    connected=$now
    want=$(wanted "$all")
    [ "$(current "$all")" = "$want" ] || layout "$want"
  done < <(niri msg --json event-stream)
}

usage() {
  echo "usage: displayctl layout laptop|external|extend | status | follow" >&2
  exit 2
}

case "${1:-}" in
  layout) layout "${2:-}" ;;
  status) status ;;
  follow) follow ;;
  *) usage ;;
esac
