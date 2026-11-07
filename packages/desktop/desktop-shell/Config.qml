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
        property real surfaceOpacity
        property real highlightOpacity
        property real dimOpacity
        property string monospaceFont
        property int fontSize
        property int radius
        property int itemRadius
        property int iconSize
        property int borderWidth
        property int spacing
        property int padding
        property int itemPaddingX
        property int itemPaddingY
        property int listSpacing
        property int edgeMargin
      }

      property JsonObject battery: JsonObject {
        property int low
        property int critical
      }

      property JsonObject menu: JsonObject {
        property var entries: []
        property int width
        property int rowsPerColumn
      }

      property JsonObject keyboard: JsonObject {
        property var labels: []
      }

      // Glyphs owned by other programs: upower-notify, volume-osd, brightness-osd, mpc-plus, awww.
      property JsonObject glyphs: JsonObject {
        property var battery: ({ discharging: [], charging: [] })
        property var audio: ({})
        property var brightness: ({})
        property var music: ({})
        property string wallpaper
      }
    }
  }
}
