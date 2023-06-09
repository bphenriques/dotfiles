#!/usr/bin/env sh

#https://github.com/FelixKratz/SketchyBar/discussions/12?sort=top#discussioncomment-3003484
SOURCE="$(defaults read ~/Library/Preferences/com.apple.HIToolbox.plist AppleCurrentKeyboardLayoutInputSourceID)"

case "${SOURCE}" in
  'com.apple.keylayout.USExtended') LABEL='US' ;;
  'com.apple.keylayout.Portuguese') LABEL='PT' ;;
esac

sketchybar --set "$NAME" label="$LABEL"
