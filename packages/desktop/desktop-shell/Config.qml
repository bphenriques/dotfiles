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
  readonly property alias display: adapter.display
  readonly property alias commands: adapter.commands
  readonly property alias shortcuts: adapter.shortcuts
  readonly property alias music: adapter.music
  readonly property alias calendar: adapter.calendar
  readonly property alias integrations: adapter.integrations

  FileView {
    path: (Quickshell.env("XDG_CONFIG_HOME") || Quickshell.env("HOME") + "/.config") + "/desktop-shell/config.json"

    JsonAdapter {
      id: adapter

      property JsonObject theme: JsonObject {
        property color background: "#1d1f21"
        property color foreground: "#c5c8c6"
        property color primary: "#81a2be"
        property color alert: "#cc6666"
        property color warning: "#f0c674"
        property real surfaceOpacity: 0.92
        property real highlightOpacity: 0.25
        property real dimOpacity: 0.5
        property string monospaceFont: "monospace"
        property int fontSize: 11
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
      }

      property JsonObject battery: JsonObject {
        property int low: 20
        property int critical: 5
      }

      property JsonObject menu: JsonObject {
        property var entries: []
        property int width: 960
        property int columns: 2
      }

      property JsonObject keyboard: JsonObject {
        property var labels: []
      }

      // Compositor keys of the header's toggles, shown while Super is held; set by the module that binds them.
      property JsonObject shortcuts: JsonObject {
        property string notifications
        property string keepAwake
        property string keyboardLayout
      }

      property JsonObject music: JsonObject {
        // The MPRIS identity of the player the Music status follows; unset, the one playing, else any.
        property string player
      }

      property JsonObject calendar: JsonObject {
        // 0 for Sunday to 6 for Saturday; the locale's by default.
        property int firstDayOfWeek: Qt.locale().firstDayOfWeek
      }

      property JsonObject osd: JsonObject {
        property int timeout: 1500
      }

      property JsonObject display: JsonObject {
        // What the Display row calls each layout.
        property var layouts: ({})
        // The laptop's panel, the screen the backlight dims.
        property string panel
      }

      // External commands, each set by the module that owns the tool, run with sh -c. One left unset shows as an empty status.
      property JsonObject commands: JsonObject {
        // One device per line: kind (sink or source), name, type and friendly name, tab separated, internal first.
        property string audioDevices
        // Given a kind and a device name, makes it the default.
        property string audioSelect
        // Prints the path of a file holding the backlight's value, a tab, and its maximum.
        property string brightness
        // Prints the display layout, then the layouts the connected screens allow, one per line.
        property string displayLayout
        // Prints true or false for do-not-disturb, then again on every change.
        property string notifications
        // Pauses notifications, or resumes them.
        property string notificationsToggle
        // Inhibits idle until its stdin closes.
        property string keepAwake
        // The OSD wheel's commands for each level, down then up: sink, source, brightness.
        property var adjust: ({})
      }

      // Optional parts by name, each { source, settings }: see README.md#integrations.
      property var integrations: ({})

      // Glyphs owned by other programs: upower-notify, audioctl, mpc-plus, display.
      property JsonObject glyphs: JsonObject {
        property var battery: ({ discharging: [], charging: [] })
        property var audio: ({})
        property var music: ({})
        property var display: ({})
      }
    }
  }
}
