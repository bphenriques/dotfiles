pragma Singleton

import QtQuick
import Quickshell
import qs

// The shell's icon set. Glyphs other programs already define arrive through Config.glyphs;
// the rest are named after their entry in the Nerd Fonts glyph list.
Singleton {
  readonly property string microphone: Config.glyphs.audio.microphone ?? ""
  readonly property string microphoneOff: Config.glyphs.audio.microphoneOff ?? ""
  readonly property string music: Config.glyphs.music.music ?? ""
  readonly property string shuffle: Config.glyphs.music.shuffle ?? ""
  readonly property string repeat: Config.glyphs.music.repeat ?? ""
  readonly property string wallpaper: Config.glyphs.wallpaper

  readonly property string wifi: String.fromCodePoint(0xF05A9) // md-wifi
  readonly property string ethernet: String.fromCodePoint(0xF0200) // md-ethernet
  readonly property string disconnected: String.fromCodePoint(0xF05AA) // md-wifi_off
  readonly property string keyboard: String.fromCodePoint(0xF030C) // md-keyboard
  readonly property string monitor: String.fromCodePoint(0xF0379) // md-monitor
  readonly property string bluetooth: String.fromCodePoint(0xF00AF) // md-bluetooth
  readonly property string bluetoothConnected: String.fromCodePoint(0xF00B1) // md-bluetooth_connect
  readonly property string bluetoothOff: String.fromCodePoint(0xF00B2) // md-bluetooth_off
  readonly property string doNotDisturb: String.fromCodePoint(0xF009B) // md-bell_off
  readonly property string tray: String.fromCodePoint(0xF1294) // md-tray
  readonly property string check: "✓"
  readonly property string separator: "➜"

  // volume-osd's device type: headphones, headset, internal or external.
  function audioDevice(type) {
    return Config.glyphs.audio[type] ?? Config.glyphs.audio.external ?? "";
  }

  // Same thresholds as packages/desktop/brightness-osd.
  function brightness(percentage) {
    return Config.glyphs.brightness[percentage < 30 ? "low" : percentage < 70 ? "medium" : "high"] ?? "";
  }

  // upower-notify's levels, one per 10%.
  function battery(percentage, pluggedIn) {
    const levels = pluggedIn ? Config.glyphs.battery.charging : Config.glyphs.battery.discharging;
    return levels[Math.min(10, Math.floor(percentage / 10))] ?? "";
  }
}
