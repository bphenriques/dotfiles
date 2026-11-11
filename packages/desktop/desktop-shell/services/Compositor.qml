pragma Singleton

import QtQuick
import Quickshell
import qs.services.compositor

// What the core asks of the compositor (see README.md#compositor), from the backend for the one running.
// Without one, the outputs are the screens Wayland reports, by connector name, and there are no keyboard layouts.
Singleton {
  id: root

  readonly property string backend: Quickshell.env("NIRI_SOCKET") ? "niri" : ""

  // Each output twice, by connector name and by make, model and serial: those connected, and those on. Read on refresh().
  readonly property var connected: internal.connected
  readonly property var enabled: internal.enabled
  readonly property var layouts: backend === "niri" ? Niri.layouts : []
  readonly property int layoutIndex: backend === "niri" ? Niri.layoutIndex : 0

  function refresh() {
    if (backend === "niri") {
      Niri.outputs((connected, enabled) => {
        internal.connected = connected;
        internal.enabled = enabled;
      });
    } else {
      internal.connected = internal.enabled = Quickshell.screens.map(screen => screen.name);
    }
  }

  function nextLayout() {
    if (backend === "niri") Niri.switchLayout();
  }

  QtObject {
    id: internal
    property var connected: []
    property var enabled: []
  }
}
