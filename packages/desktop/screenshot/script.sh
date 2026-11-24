#shellcheck shell=bash

# $1 is the destination as a strftime pattern, like niri's screenshot-path.
destination="$(date +"$1")"
mkdir -p -- "$(dirname -- "$destination")"
tmp="$(mktemp --suffix=.png)"
trap 'rm -f -- "$tmp"' EXIT

# niri fills the clipboard after its reply: clear it first, then wait for the image, else an older one is read.
from_niri() {
  wl-copy --clear
  niri msg action "$@"
  for _ in $(seq 50); do
    wl-paste --list-types 2>/dev/null | grep -qx image/png && break
    sleep 0.1
  done
  wl-paste --type image/png >"$tmp"
}

case "${2:-screen}" in
  screen)
    from_niri screenshot-screen --write-to-disk=false --show-pointer=false
    ;;
  region)
    geometry="$(slurp)" || exit 0
    grim -g "$geometry" "$tmp"
    ;;
  window)
    from_niri screenshot-window --write-to-disk=false
    ;;
  *)
    printf 'unknown mode: %s\n' "${2:-}" >&2
    exit 2
    ;;
esac

mv -- "$tmp" "$destination"
wl-copy --type image/png <"$destination"

# Its actions, Delete sent first: makoctl menu lists the last sent first, so Edit (a click) tops it.
# No replace tag: mako would hand a later answer to this waiting process too.
action=$(notify-send \
  --app-name "Screenshot" \
  --category "screenshot" \
  --expire-time 15000 \
  --icon "$destination" \
  --action "delete=Delete" \
  --action "folder=Open folder" \
  --action "copy=Copy file" \
  --action "default=Edit" \
  "Screenshot" \
  "$(basename "$destination")" 2>/dev/null || true)

case "$action" in
  default) satty -f "$destination" --output-filename "$destination" ;;
  # As a file, so a chat app attaches it; the image itself was copied at capture.
  copy) wl-copy --type text/uri-list "file://$destination" ;;
  folder) xdg-open "$(dirname -- "$destination")" >/dev/null 2>&1 || true ;;
  delete)
    gio trash -- "$destination" &&
      notify-send --app-name "Screenshot" --urgency low --icon user-trash "Moved to trash" "$(basename "$destination")"
    ;;
esac
