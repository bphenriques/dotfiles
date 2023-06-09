#!/usr/bin/env bash

# shellcheck disable=SC1091
. "$HOME/.config/sketchybar/theme.sh"

# Resoruces:
# - Plugins: https://github.com/FelixKratz/SketchyBar/discussions/12
# - Setups: https://github.com/FelixKratz/SketchyBar/discussions/47
# - Notifications: https://github.com/FelixKratz/SketchyBar/discussions/151
# - Nerd Font Icons: https://www.nerdfonts.com/cheat-sheet

PLUGIN_DIR="$HOME/.config/sketchybar/plugins"

bar=(
  color="$BACKGROUND"
  height=32
  sticky=off
  padding_left=0
  padding_right=0
  shadow=off
  topmost=off
)
default=(
  updates=when_shown
  drawing=on
  icon.font="$FONT_FACE:Bold:15.0"
  icon.color="$FOREGROUND"
  icon.padding_left=6
  icon.padding_right=6
  label.padding_right=6
  label.font="$FONT_FACE:Bold:13.0"
  label.color="$FOREGROUND"
)
sketchybar \
  --bar "${bar[@]}" \
  --default "${default[@]}"

SPACE_ICONS=("1" "2" "3" "4" "5" "6" "7" "8" "9")
spaces=(
  icon.font="$FONT_FACE:Regular:14.0"
  icon.padding_left=10
  icon.padding_right=10
  icon.align=center
  label.padding_left=0
  label.padding_right=0
  padding_right=10
  script="$PLUGIN_DIR/space.sh"
  background.corner_radius=4
  background.height=26
)
sid=0
for i in "${!SPACE_ICONS[@]}"; do
  sid=$((i + 1))
  sketchybar --add space space.$sid left \
             --set space.$sid associated_space=$sid "${spaces[@]}" \
                              icon="${SPACE_ICONS[i]}" \
                              click_script="echo 'tell application \"System Events\" to key code $((17 + sid)) using control down' | osascript"
done

# Deal with rounded screen
sketchybar \
  --set space.1 padding_left=18 \
  --add item app_name left \
    --set app_name script="${PLUGIN_DIR}/front_app.sh" \
    --subscribe app_name front_app_switched

clock=(
  background.color="$YELLOW"
  label.color="$BLACK"
  update_freq=30
  label.padding_right=16 # Deal with rounded screen
  script="$PLUGIN_DIR/clock.sh"
  padding_left=5
)
weather=(
  label.color="$FOREGROUND"
  icon.color="$YELLOW"
  script="$PLUGIN_DIR/weather.sh"
  click_script="sketchybar --set weather popup.drawing=toggle"
  update_freq=1800
)
battery=(
  script="$PLUGIN_DIR/battery.sh"
  icon.font="$FONT_FACE:Regular:19.0"
  update_freq=120
)
volume=(
  script="$PLUGIN_DIR/volume.sh"
  icon.font="$FONT_FACE:Regular:18.0"
  icon.width=20
)
wifi=(
  icon.font="$FONT_FACE:Regular:18.0"
  label.padding_left=0
  label.padding_right=0
  script="$PLUGIN_DIR/wifi.sh"
  update_freq=5
)
vpn=(
  icon.font="$FONT_FACE:Regular:18.0"
  script="$PLUGIN_DIR/vpn.sh"
)
stats=(
  background.corner_radius=4
  background.color=0xff3f444a
)
ram=(
  icon=
  icon.font="$FONT_FACE:Regular:15.0"
  update_freq=5
  script="$PLUGIN_DIR/ram.sh"
  padding_right=0
)
cpu=(
  icon=󰍛
  icon.font="$FONT_FACE:Regular:19.0"
  update_freq=5
  script="$PLUGIN_DIR/cpu.sh"
  padding_right=0
)
bluetooth=(
  icon.font="$FONT_FACE:Regular:19.0"
  label.padding_right=0
  script="$PLUGIN_DIR/bluetooth.sh"
)
language=(
  click_script="echo 'tell application \"System Events\" to key code 49 using control down' | osascript"
  script="$PLUGIN_DIR/keyboard_layout.sh"
  icon.padding_left=0
)

sketchybar \
  --add item clock right \
    --set clock "${clock[@]}" \
    --subscribe clock system_woke \
  --add item battery right \
    --set battery "${battery[@]}" \
    --subscribe battery power_source_change system_woke \
  --add item volume right \
    --set volume "${volume[@]}" \
    --subscribe volume volume_change \
  --add item wifi right \
    --set wifi "${wifi[@]}" \
    --subscribe wifi system_woke \
  --add item vpn right \
    --add event vpn_status "com.apple.networkextension.statuschanged" \
    --set vpn "${vpn[@]}" \
    --subscribe vpn system_woke vpn_status \
  --add item language right \
    --add event input_source AppleSelectedInputSourcesChangedNotification \
    --set language "${language[@]}" \
    --subscribe language system_woke input_source \
  --add item bluetooth right \
    --add event bluetooth_status "com.apple.bluetooth.status" \
    --add event bluetooth_on "IOBluetoothHostControllerPoweredOnNotification" \
    --add event bluetooth_off "IOBluetoothHostControllerPoweredOffNotification" \
    --set bluetooth "${bluetooth[@]}" \
    --subscribe bluetooth system_woke bluetooth_status bluetooth_off bluetooth_on \
  --add item ram right \
    --set ram "${ram[@]}" \
  --add item cpu right \
    --set cpu "${cpu[@]}" \
  --add bracket stats ram cpu \
    --set stats "${stats[@]}" \
  --add item weather right \
    --set weather "${weather[@]}" \
    --subscribe weather system_woke

sketchybar --update
sketchybar --trigger space_change
