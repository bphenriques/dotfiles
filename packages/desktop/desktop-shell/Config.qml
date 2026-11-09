pragma Singleton

import QtQuick
import Quickshell
import Quickshell.Io

// Typed view of $XDG_CONFIG_HOME/desktop-shell/config.json, written by modules/home-manager/programs/desktop-shell.nix.
Singleton {
  readonly property alias theme: adapter.theme
  readonly property alias battery: adapter.battery
  readonly property alias menu: adapter.menu
  readonly property alias keyboard: adapter.keyboard
  readonly property alias glyphs: adapter.glyphs
  readonly property alias osd: adapter.osd
  readonly property alias commands: adapter.commands

  FileView {
    path: (Quickshell.env("XDG_CONFIG_HOME") || Quickshell.env("HOME") + "/.config") + "/desktop-shell/config.json"

    JsonAdapter {
      id: adapter

      property JsonObject theme: JsonObject {
        property color background
        property color foreground
        property color primary
        property color alert
        property color warning
        property real surfaceOpacity: 0.92
        property real highlightOpacity: 0.25
        property real dimOpacity: 0.5
        property string monospaceFont
        property int fontSize
        property int radius: 10
        property int itemRadius: 6
        property int iconSize: 24
        property int borderWidth: 2
        property int spacing: 8
        property int padding: 14
        property int itemPaddingX: 8
        property int itemPaddingY: 5
        property int listSpacing: 0
        property int edgeMargin: 35
        property int osdLength: 160
        property int osdIconSize: 28
        property int meterThickness: 6
        property int meterLength: 48
        property int minimapHeight: 30
        property int minimapMaxWidth: 180
        property real minimapOpacity: 0.7
        property int minimapBorderWidth: 1
      }

      property JsonObject battery: JsonObject {
        property int low
        property int critical
      }

      property JsonObject menu: JsonObject {
        property var entries: []
        property int width: 960
        property int columns: 2
      }

      property JsonObject keyboard: JsonObject {
        property var labels: []
      }


      property JsonObject osd: JsonObject {
        property int timeout: 1500
        // Commands the wheel runs on each level, down then up: sink, source, brightness.
        property var adjust: ({})
      }

      // External commands, each set by the module that owns the tool, run with sh -c. One left unset shows as an empty status.
      property JsonObject commands: JsonObject {
        // One device per line: kind (sink or source), name, type and friendly name, tab separated, internal first.
        property string audioDevices
        // Given a kind and a device name, makes it the default.
        property string audioSelect
        // Prints the output's type (headphones, headset, internal, external), a tab, and its friendly name.
        property string audioDevice
        // Prints the path of a file holding the backlight's value, a tab, and its maximum.
        property string brightness
        // Prints the current display profile.
        property string displayProfile
        // niri msg --json outputs.
        property string outputs
        // Prints true or false for do-not-disturb, then again on every change.
        property string notifications
        // Inhibits idle until its stdin closes.
        property string keepAwake
        property string recordingStop
      }

      // Glyphs owned by other programs: upower-notify, volume-osd, mpc-plus.
      property JsonObject glyphs: JsonObject {
        property var battery: ({ discharging: [], charging: [] })
        property var audio: ({})
        property var music: ({})
      }
    }
  }
}
