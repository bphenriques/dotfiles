pragma Singleton

import QtQuick
import Quickshell
import qs

// The shell's icon set. Glyphs other programs already define arrive in their feature's glyphs (Config.audio.glyphs);
// the rest are named after their entry in the Nerd Fonts glyph list.
Singleton {
  readonly property string microphone: Config.audio.glyphs.microphone ?? ""
  readonly property string microphoneOff: Config.audio.glyphs.microphoneOff ?? ""
  readonly property string music: Config.music.glyphs.music ?? ""
  readonly property string shuffle: Config.music.glyphs.shuffle ?? ""
  readonly property string repeat: Config.music.glyphs.repeat ?? ""

  readonly property string wifi: "\u{F05A9}" // md-wifi
  readonly property string ethernet: "\u{F0200}" // md-ethernet
  readonly property string disconnected: "\u{F05AA}" // md-wifi_off
  readonly property string volumeOff: "\u{F075F}" // md-volume_off
  readonly property string keyboard: "\u{F030C}" // md-keyboard
  readonly property string monitor: "\u{F0379}" // md-monitor
  readonly property string bluetooth: "\u{F00AF}" // md-bluetooth
  readonly property string bluetoothConnected: "\u{F00B1}" // md-bluetooth_connect
  readonly property string bluetoothOff: "\u{F00B2}" // md-bluetooth_off
  readonly property string notifications: "\u{F009A}" // md-bell
  readonly property string doNotDisturb: "\u{F009B}" // md-bell_off
  readonly property string keepAwake: "\u{F0176}" // md-coffee
  readonly property string tray: "\u{F003B}" // md-apps
  readonly property string wallpaper: "\u{F0E09}" // md-wallpaper
  readonly property string window: "\u{F0614}" // md-application_outline
  readonly property string record: "\u{F044B}" // md-record_rec
  readonly property string calendarDay: "\u{F00EE}" // md-calendar_blank
  readonly property string calendarWeek: "\u{F0A33}" // md-calendar_week
  readonly property string calendarMonth: "\u{F0E17}" // md-calendar_month
  readonly property string calendarYear: "\u{F0679}" // md-calendar_range
  readonly property string calendarToday: "\u{F00F6}" // md-calendar_today
  readonly property var brightnessLevels: ["\u{F00DE}", "\u{F00DF}", "\u{F00E0}"] // md-brightness_5, _6, _7
  readonly property string check: "✓"

  // The display layout: laptop, external or extend.
  function display(layout) {
    return Config.display.glyphs[layout] ?? monitor;
  }

  // audioctl's device type: headphones, headset, internal, external or microphone.
  function audioDevice(type) {
    return Config.audio.glyphs[type] ?? Config.audio.glyphs.external ?? "";
  }

  function brightness(percentage) {
    return brightnessLevels[percentage < 30 ? 0 : percentage < 70 ? 1 : 2];
  }

  // upower-notify's levels, one per 10%.
  function battery(percentage, pluggedIn) {
    const levels = pluggedIn ? Config.battery.glyphs.charging : Config.battery.glyphs.discharging;
    return levels[Math.min(10, Math.floor(percentage / 10))] ?? "";
  }
}
