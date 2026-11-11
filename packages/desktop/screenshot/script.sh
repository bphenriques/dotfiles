#shellcheck shell=bash

# $1 is the destination as a strftime pattern, like niri's screenshot-path.
destination="$(date +"$1")"
mkdir -p -- "$(dirname -- "$destination")"
tmp="$(mktemp --suffix=.png)"
trap 'rm -f -- "$tmp"' EXIT

case "${2:-screen}" in
  screen)
    niri msg action screenshot-screen --write-to-disk=false --show-pointer=false
    wl-paste --type image/png >"$tmp"
    ;;
  region)
    geometry="$(slurp)" || exit 0
    grim -g "$geometry" "$tmp"
    ;;
  window)
    niri msg action screenshot-window --write-to-disk=false
    wl-paste --type image/png >"$tmp"
    ;;
  *)
    printf 'unknown mode: %s\n' "${2:-}" >&2
    exit 2
    ;;
esac

mv -- "$tmp" "$destination"
wl-copy --type image/png <"$destination"

# Notify with an edit action that opens image editor.
action=$(notify-send \
  --category "screenshot" \
  --hint string:x-canonical-private-synchronous:screenshot \
  --action "default=Edit" \
  --expire-time 10000 \
  --icon "$destination" \
  "Screenshot" \
  "$(basename "$destination")" 2>/dev/null || true)

if [ "$action" = "default" ]; then
  satty -f "$destination" --output-filename "$destination"
fi
