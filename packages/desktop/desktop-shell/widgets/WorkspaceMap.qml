pragma ComponentBehavior: Bound

import QtQuick
import Quickshell
import Quickshell.Wayland
import qs
import qs.services
import qs.ui

// A minimap in the corner when you move to another window or workspace: the non-empty workspaces top to bottom, then the active one's columns to scale (none when empty),
// the focused window lit, between thin strips of the workspaces above and below.
// niri does not report how far a workspace is scrolled, so the map shows every column rather than the visible ones.
Scope {
  id: root

  property bool shown: false
  property real fade: shown ? 1 : 0
  Behavior on fade {
    NumberAnimation {
      duration: 150
    }
  }

  readonly property var screen: Quickshell.screens.find(screen => screen.name === Niri.map?.output) ?? null
  readonly property int gap: 2 * Config.theme.minimapBorderWidth
  readonly property real dot: Config.theme.minimapHeight / 8
  readonly property real strip: Config.theme.minimapHeight / 5
  // Sized to minimapHeight for the output's height, shrunk further if any of the three workspaces would pass minimapMaxWidth.
  readonly property real scale: {
    if (!screen || !Niri.map) return 0;
    let fit = Config.theme.minimapHeight / screen.height;
    for (const columns of [Niri.map.above, Niri.map.columns, Niri.map.below]) {
      const total = columns.reduce((sum, column) => sum + widthOf(column), 0);
      if (total > 0) fit = Math.min(fit, (Config.theme.minimapMaxWidth - gap * (columns.length - 1)) / total);
    }
    return fit;
  }

  function widthOf(column) {
    return Math.max(...column.map(window => window.width));
  }

  QtObject {
    id: state
    readonly property bool armed: Startup.settled
  }

  Timer {
    id: hideTimer
    interval: Config.osd.timeout
    onTriggered: root.shown = false
  }

  Connections {
    target: Niri
    function onMoved() {
      if (Niri.overview || !state.armed) return;
      root.shown = true;
      hideTimer.restart();
    }
    function onOverviewChanged() {
      if (Niri.overview) root.shown = false;
    }
  }

  // A window, its app's icon in the middle; the focused one outlined in the primary colour.
  component Tile: Rectangle {
    id: tile

    property bool focused: false
    property string appId: ""

    radius: Config.theme.itemRadius / 2
    color: Tone.surface(Config.theme.minimapOpacity)
    border.width: Config.theme.minimapBorderWidth
    border.color: Tone.color(focused ? "primary" : "dim")

    Icon {
      anchors.centerIn: parent
      visible: tile.appId !== ""
      size: Math.min(0.4 * Config.theme.minimapHeight, 0.6 * Math.min(tile.width, tile.height))
      source: tile.appId ? Quickshell.iconPath(DesktopEntries.heuristicLookup(tile.appId)?.icon ?? tile.appId, true) : ""
      glyph: Glyphs.window
      tone: tile.focused ? "normal" : "dim"
    }
  }

  // A neighbouring workspace: one block per column, without windows or icons; the column focus would land on is outlined.
  component Strip: Row {
    id: strip

    property var columns: []

    visible: columns.length > 0
    spacing: root.gap

    Repeater {
      model: strip.columns

      Tile {
        required property var modelData
        readonly property bool target: modelData.some(window => window.focused)

        width: root.widthOf(modelData) * root.scale
        height: root.strip
        focused: target
      }
    }
  }

  LazyLoader {
    active: root.fade > 0 && root.screen !== null

    OverlayWindow {
      screen: root.screen
      anchors.top: true
      anchors.left: true
      margins.top: Config.theme.edgeMargin
      margins.left: Config.theme.edgeMargin
      WlrLayershell.namespace: "workspace-map"
      mask: Region {}

      implicitWidth: content.implicitWidth
      implicitHeight: content.implicitHeight

      Row {
        id: content
        opacity: root.fade
        spacing: Config.theme.spacing

        // niri stacks workspaces vertically; the active one is the long mark.
        Column {
          anchors.verticalCenter: parent.verticalCenter
          spacing: root.dot

          Repeater {
            model: Niri.map.workspaces

            Rectangle {
              required property var modelData

              width: root.dot
              height: modelData.active ? 3 * root.dot : root.dot
              radius: root.dot / 2
              color: Tone.color(modelData.active ? "primary" : "dim")
            }
          }
        }

        Column {
          anchors.verticalCenter: parent.verticalCenter
          spacing: root.gap

          Strip {
            columns: Niri.map.above
          }

          Row {
            spacing: root.gap

            Repeater {
              model: Niri.map.columns

              Column {
                id: column
                required property var modelData

                spacing: root.gap

                Repeater {
                  model: column.modelData

                  Tile {
                    required property var modelData

                    width: root.widthOf(column.modelData) * root.scale
                    height: modelData.height * root.scale
                    focused: modelData.focused
                    appId: modelData.appId
                  }
                }
              }
            }
          }

          Strip {
            columns: Niri.map.below
          }
        }
      }
    }
  }
}
