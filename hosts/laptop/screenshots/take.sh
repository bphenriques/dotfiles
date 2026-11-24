# Retakes this folder's screenshots on the live niri desktop: quick-key as a second instance with made-up events and a
# canned assistant (quick-key's docs-shots), over a terminal showing fastfetch, on an empty workspace.
# Usage: bash take.sh <quick-key checkout>
set -euo pipefail
pgrep -x hyprlock >/dev/null && { echo "Session locked: unlock first." >&2; exit 1; }

here=$(dirname "$(realpath "$0")")
qk=$(realpath "${1:?usage: take.sh <quick-key checkout>}")
docs=$qk/.agents/skills/docs-shots/scripts
work=$(mktemp -d)
export XDG_RUNTIME_DIR=${XDG_RUNTIME_DIR:-/run/user/$(id -u)} WAYLAND_DISPLAY=${WAYLAND_DISPLAY:-wayland-1}
NIRI_SOCKET=${NIRI_SOCKET:-$(ls "$XDG_RUNTIME_DIR"/niri.*.sock | head -1)}
export NIRI_SOCKET
python=$(nix build --no-link --print-out-paths --impure --expr '(builtins.getFlake "nixpkgs").legacyPackages.${builtins.currentSystem}.python3.withPackages (ps: [ ps.agent-client-protocol ])')/bin/python3
fastfetch=$(nix build --no-link --print-out-paths "nixpkgs#fastfetch^out")/bin/fastfetch

jq '
  def date(d): now + d * 86400 | localtime | strftime("%Y-%m-%d");
  { updated: (now | localtime | strftime("%Y-%m-%dT%H:%M")),
    events: map(
      { title, color, location }
      + if .start then { allDay: false, start: "\(date(.day))T\(.start)", end: "\(date(.day))T\(.end)" }
        else { allDay: true, start: date(.day), end: date(.day + (.days // 1) - 1) } end
      | with_entries(select(.value != null))) }
' "$qk/.agents/skills/record-tour/scripts/events.json" > "$work/events.json"

# The live config with those events, no Wi-Fi entry (its status names the network) and the canned assistant.
mkdir -p "$work/config/quick-key"
ln -s "${XDG_CONFIG_HOME:-$HOME/.config}/fontconfig" "$work/config/fontconfig"
ln -s "${XDG_CONFIG_HOME:-$HOME/.config}/quick-key/themes" "$work/config/quick-key/themes"
jq --arg file "$work/events.json" --arg agent "$python $docs/agent.py" '
  .calendar.file = $file | del(.calendar.remind) | .menu.entries |= map(select(.list != "wifi"))
  | .plugins.ask.ask = $agent
' "${XDG_CONFIG_HOME:-$HOME/.config}/quick-key/config.json" > "$work/config/quick-key/config.json"

output=$(niri msg --json focused-output | jq -r .name)
back=$(niri msg --json workspaces | jq '.[] | select(.is_focused) | .idx')
empty=$(niri msg --json workspaces | jq --arg o "$output" '[.[] | select(.output == $o and .active_window_id == null)] | max_by(.idx) | .idx')
niri msg action focus-workspace "$empty"
# As the old shots had it: no host model, display, disk, network or battery.
ghostty +new-window --title=dotfiles-shots --working-directory="$HOME" -e sh -c "clear; $fastfetch --structure Title:Separator:OS:Kernel:Uptime:Packages:Shell:WM:Terminal:CPU:GPU:Memory:Break:Colors; exec zsh" &
sleep 3
window=$(niri msg --json windows | jq '.[] | select(.title == "dotfiles-shots") | .id')

cp -r --no-preserve=mode "$qk/packages/quick-key" "$work/shell"
XDG_CONFIG_HOME=$work/config quickshell -p "$work/shell" > "$work/qs.log" 2>&1 &
pid=$!
trap 'quickshell kill --pid $pid >/dev/null 2>&1 || kill $pid; [ -n "$window" ] && niri msg action close-window --id "$window"; niri msg action focus-workspace "$back"; rm -rf "$work"' EXIT
wlrctl pointer move -100000 -100000
sleep 4

shot() { sleep 1.5; grim -s 1 -o "$output" "$here/$1.png"; }
ipc() { quickshell ipc --pid "$pid" call "$@"; }

ipc menu toggle; shot menu-top; ipc menu toggle; sleep 0.5
ipc menu toggleEntry c; shot menu-calendar; ipc menu toggleEntry c; sleep 0.5
ipc ask activate; sleep 1
wtype -k Shift_L
wtype "What's on this week?"; wtype -k Return
shot ask
ipc ask activate
ls -la "$here"/*.png
