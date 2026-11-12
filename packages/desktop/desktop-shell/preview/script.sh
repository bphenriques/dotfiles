# Runs a working tree of the shell in a nested niri, then opens a shell inside it to drive it.
dir=$(realpath "${1:?usage: desktop-shell-preview <shell dir> [config.json]}")
config_home=${XDG_CONFIG_HOME:-$HOME/.config}
config=$(realpath "${2:-$config_home/desktop-shell/config.json}")
: "${WAYLAND_DISPLAY:?run inside a Wayland session}"

work=$(mktemp -d)
trap 'kill $(jobs -p) 2>/dev/null; rm -rf "$work"' EXIT

# Your config, for fonts and icons, with this config.json in place of the shell's.
mkdir "$work/config" "$work/config.shell"
ln -s "$config_home"/* "$work/config/"
rm -f "$work/config/desktop-shell"
mv "$work/config.shell" "$work/config/desktop-shell"
cp "$config" "$work/config/desktop-shell/config.json"

# niri runs the command once it is up, which reports where; the read waits for it, opened read-write so the open itself cannot block.
mkfifo "$work/ready"
exec 3<>"$work/ready"
# shellcheck disable=SC2016 # expanded by niri's child, inside the nested session
niri -c "$PREVIEW_NIRI_CONFIG" -- sh -c 'printf "%s %s\n" "$WAYLAND_DISPLAY" "$NIRI_SOCKET" > "$1"' sh "$work/ready" > "$work/niri.log" 2>&1 &
if ! read -r -t 30 -u 3 display socket; then
  echo "nested niri did not start; its log:" >&2
  cat "$work/niri.log" >&2
  exit 1
fi
export WAYLAND_DISPLAY=$display NIRI_SOCKET=$socket

XDG_CONFIG_HOME=$work/config quickshell -p "$dir" > "$work/quickshell.log" 2>&1 &

cat <<HELP
Shell running in the nested niri; its log: $work/quickshell.log. Exit this shell to stop both.
  quickshell -p $dir ipc call menu toggle
  wtype a                                 # a key; wtype -P Super_L -s 1500 -p Super_L holds one
  wlrctl pointer move -5000 -5000         # to the top left, then relative moves; wlrctl pointer click left
  grim shot.png
Session actions run for real.
HELP
"${SHELL:-bash}"
