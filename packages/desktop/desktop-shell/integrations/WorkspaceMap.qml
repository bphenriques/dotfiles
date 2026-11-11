pragma ComponentBehavior: Bound

import QtQuick
import Quickshell
import Quickshell.Wayland
import qs
import qs.services
import qs.services.compositor
import qs.ui

// A minimap in the corner when you move to another window or workspace: the non-empty workspaces top to bottom, then the active one's columns to scale (none when empty),
// the focused window lit, between thin strips of the workspaces above and below.
// niri does not report how far a workspace is scrolled, so the map shows every column rather than the visible ones.
// settings: height, maxWidth, opacity and borderWidth of the map, in pixels but for opacity.
Integration {
  id: root

  readonly property int mapHeight: settings.height ?? 30
  readonly property int maxWidth: settings.maxWidth ?? 180
  readonly property real tileOpacity: settings.opacity ?? 0.7
  readonly property int borderWidth: settings.borderWidth ?? 1

  property bool overview: false
  // On the focused output: its non-empty workspaces in order { active }, then the columns of the active one and of the workspaces above and below it,
  // each column its windows top to bottom { appId, width, height, focused }; focused is the window focus lands on in that workspace.
  // Changes only when the layout does: titles are left out.
  property var map: null

  property real fade: transient.shown ? 1 : 0
  Behavior on fade {
    NumberAnimation {
      duration: 150
    }
  }

  readonly property var screen: Quickshell.screens.find(screen => screen.name === root.map?.output) ?? null
  readonly property int gap: 2 * borderWidth
  readonly property real dot: root.mapHeight / 8
  readonly property real strip: root.mapHeight / 5
  // Sized to mapHeight for the output's height, shrunk further if any of the three workspaces would pass maxWidth.
  readonly property real scale: {
    if (!screen || !root.map) return 0;
    let fit = root.mapHeight / screen.height;
    for (const columns of [root.map.above, root.map.columns, root.map.below]) {
      const total = columns.reduce((sum, column) => sum + widthOf(column), 0);
      if (total > 0) fit = Math.min(fit, (root.maxWidth - gap * (columns.length - 1)) / total);
    }
    return fit;
  }

  function widthOf(column) {
    return Math.max(...column.map(window => window.width));
  }

  Transient {
    id: transient
    armed: Startup.settled
  }

  // You moved: focus went to another window or workspace. A window that opens or closes moves focus without it.
  function moved() {
    if (!overview) transient.show();
  }

  QtObject {
    id: tracked
    property var workspaces: ({})
    property var windows: ({})
  }

  function columnsOf(workspace) {
    const columns = [];
    for (const window of Object.values(tracked.windows)) {
      const position = window.layout?.pos_in_scrolling_layout;
      if (!position || window.workspace_id !== workspace?.id) continue;
      const [column, row] = position;
      const [width, height] = window.layout.tile_size;
      if (!columns[column - 1]) columns[column - 1] = [];
      columns[column - 1][row - 1] = { appId: window.app_id ?? "", width: Math.round(width), height: Math.round(height), focused: window.id === workspace.active_window_id };
    }
    // Holes appear for a moment while windows move between columns.
    return Array.from(columns, column => (column ?? []).filter(window => window)).filter(column => column.length > 0);
  }

  function update() {
    const workspaces = Object.values(tracked.workspaces);
    const focused = workspaces.find(workspace => workspace.is_focused);
    if (!focused) return;
    const onOutput = workspaces.filter(workspace => workspace.output === focused.output).sort((a, b) => a.idx - b.idx);
    const index = onOutput.findIndex(workspace => workspace.is_active);
    const occupied = new Set(Object.values(tracked.windows).map(window => window.workspace_id));
    const next = {
      output: focused.output,
      workspaces: onOutput.filter(workspace => workspace.is_active || occupied.has(workspace.id)).map(workspace => ({ active: workspace.is_active })),
      above: columnsOf(onOutput[index - 1]),
      columns: columnsOf(onOutput[index]),
      below: columnsOf(onOutput[index + 1])
    };
    if (JSON.stringify(next) !== JSON.stringify(map)) map = next;
  }

  function byId(list) {
    const items = {};
    for (const item of list) items[item.id] = item;
    return items;
  }

  function handle(event) {
    if (event.WorkspacesChanged) {
      tracked.workspaces = byId(event.WorkspacesChanged.workspaces);
    } else if (event.WorkspaceActivated) {
      const activated = tracked.workspaces[event.WorkspaceActivated.id];
      for (const workspace of Object.values(tracked.workspaces)) {
        if (workspace.output === activated?.output) workspace.is_active = workspace.id === activated.id;
        if (event.WorkspaceActivated.focused) workspace.is_focused = workspace.id === activated?.id;
      }
    } else if (event.WorkspaceActiveWindowChanged) {
      const workspace = tracked.workspaces[event.WorkspaceActiveWindowChanged.workspace_id];
      if (workspace) workspace.active_window_id = event.WorkspaceActiveWindowChanged.active_window_id;
    } else if (event.WindowFocusChanged) {
      pendingMove.restart();
      return;
    } else if (event.WindowsChanged) {
      tracked.windows = byId(event.WindowsChanged.windows);
    } else if (event.WindowOpenedOrChanged) {
      const window = event.WindowOpenedOrChanged.window;
      tracked.windows[window.id] = window;
    } else if (event.WindowClosed) {
      pendingMove.stop();
      delete tracked.windows[event.WindowClosed.id];
    } else if (event.WindowLayoutsChanged) {
      for (const [id, layout] of event.WindowLayoutsChanged.changes) if (tracked.windows[id]) tracked.windows[id].layout = layout;
    } else if (event.OverviewOpenedOrClosed) {
      overview = event.OverviewOpenedOrClosed.is_open;
      if (overview) transient.hide();
      return;
    } else {
      return;
    }
    update();
    if (event.WorkspaceActivated?.focused) pendingMove.restart();
  }

  // Closing a window moves focus first and reports the close right after: a move waits a moment to see whether it was one.
  Timer {
    id: pendingMove
    interval: 50
    onTriggered: root.moved()
  }

  Connections {
    target: Niri

    function onReceived(event) {
      root.handle(event);
    }
  }

  // A window, its app's icon in the middle; the focused one outlined in the primary colour.
  component Tile: Rectangle {
    id: tile

    property bool focused: false
    property string appId: ""

    radius: Config.theme.itemRadius / 2
    color: Tone.surface(root.tileOpacity)
    border.width: borderWidth
    border.color: Tone.color(focused ? "primary" : "dim")

    Icon {
      anchors.centerIn: parent
      visible: tile.appId !== ""
      size: Math.min(0.4 * root.mapHeight, 0.6 * Math.min(tile.width, tile.height))
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
            model: root.map.workspaces

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
            columns: root.map.above
          }

          Row {
            spacing: root.gap

            Repeater {
              model: root.map.columns

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
            columns: root.map.below
          }
        }
      }
    }
  }
}
