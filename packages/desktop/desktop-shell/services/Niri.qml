pragma Singleton

import QtQuick
import Quickshell
import Quickshell.Io

// niri's workspaces, tiled windows and keyboard layouts, followed through its event stream, which replays everything on connect.
// `map` changes only when the layout does: titles are left out.
Singleton {
  id: root

  property bool overview: false
  property var layouts: []
  property int layoutIndex: 0
  // On the focused output: its non-empty workspaces in order { active }, then the columns of the active one and of the workspaces above and below it,
  // each column its windows top to bottom { appId, width, height, focused }; focused is the window focus lands on in that workspace.
  property var map: null
  // You moved: focus went to another window or workspace. A window that opens or closes moves focus without it.
  signal moved()

  QtObject {
    id: state
    property var workspaces: ({})
    property var windows: ({})
  }

  function columnsOf(workspace) {
    const columns = [];
    for (const window of Object.values(state.windows)) {
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
    const workspaces = Object.values(state.workspaces);
    const focused = workspaces.find(workspace => workspace.is_focused);
    if (!focused) return;
    const onOutput = workspaces.filter(workspace => workspace.output === focused.output).sort((a, b) => a.idx - b.idx);
    const index = onOutput.findIndex(workspace => workspace.is_active);
    const occupied = new Set(Object.values(state.windows).map(window => window.workspace_id));
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
      state.workspaces = byId(event.WorkspacesChanged.workspaces);
    } else if (event.WorkspaceActivated) {
      const activated = state.workspaces[event.WorkspaceActivated.id];
      for (const workspace of Object.values(state.workspaces)) {
        if (workspace.output === activated?.output) workspace.is_active = workspace.id === activated.id;
        if (event.WorkspaceActivated.focused) workspace.is_focused = workspace.id === activated?.id;
      }
    } else if (event.WorkspaceActiveWindowChanged) {
      const workspace = state.workspaces[event.WorkspaceActiveWindowChanged.workspace_id];
      if (workspace) workspace.active_window_id = event.WorkspaceActiveWindowChanged.active_window_id;
    } else if (event.WindowFocusChanged) {
      pendingMove.restart();
      return;
    } else if (event.WindowsChanged) {
      state.windows = byId(event.WindowsChanged.windows);
    } else if (event.WindowOpenedOrChanged) {
      const window = event.WindowOpenedOrChanged.window;
      state.windows[window.id] = window;
    } else if (event.WindowClosed) {
      pendingMove.stop();
      delete state.windows[event.WindowClosed.id];
    } else if (event.WindowLayoutsChanged) {
      for (const [id, layout] of event.WindowLayoutsChanged.changes) if (state.windows[id]) state.windows[id].layout = layout;
    } else if (event.KeyboardLayoutsChanged) {
      root.layouts = event.KeyboardLayoutsChanged.keyboard_layouts.names;
      root.layoutIndex = event.KeyboardLayoutsChanged.keyboard_layouts.current_idx;
      return;
    } else if (event.KeyboardLayoutSwitched) {
      root.layoutIndex = event.KeyboardLayoutSwitched.idx;
      return;
    } else if (event.OverviewOpenedOrClosed) {
      root.overview = event.OverviewOpenedOrClosed.is_open;
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

  Socket {
    path: Quickshell.env("NIRI_SOCKET")
    connected: true
    onConnectedChanged: if (connected) write('"EventStream"\n')

    parser: SplitParser {
      onRead: line => root.handle(JSON.parse(line))
    }
  }
}
