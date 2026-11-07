pragma Singleton

import QtQuick
import Quickshell
import Quickshell.Io

// Connected only while subscribed: niri sends an event for every window title change.
// niri replays its full state on connect, so nothing is missed between subscriptions.
Singleton {
  id: root

  property int subscribers: 0
  property var keyboardLayouts: []
  property int keyboardLayoutIndex: 0
  readonly property string keyboardLayout: keyboardLayouts[keyboardLayoutIndex] ?? ""

  Socket {
    path: Quickshell.env("NIRI_SOCKET")
    connected: root.subscribers > 0
    onConnectedChanged: if (connected) write('"EventStream"\n')

    parser: SplitParser {
      onRead: line => {
        const event = JSON.parse(line);
        if (event.KeyboardLayoutsChanged) {
          root.keyboardLayouts = event.KeyboardLayoutsChanged.keyboard_layouts.names;
          root.keyboardLayoutIndex = event.KeyboardLayoutsChanged.keyboard_layouts.current_idx;
        } else if (event.KeyboardLayoutSwitched) {
          root.keyboardLayoutIndex = event.KeyboardLayoutSwitched.idx;
        }
      }
    }
  }
}
