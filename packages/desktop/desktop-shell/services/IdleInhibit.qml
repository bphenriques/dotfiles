pragma Singleton

import QtQuick
import Quickshell
import Quickshell.Io
import qs

// Keeps the screen awake with Config.commands.keepAwake, a systemd idle inhibitor hypridle honours. Off again when the shell restarts.
Singleton {
  id: root

  readonly property bool enabled: internal.enabled

  function toggle() {
    internal.enabled = !internal.enabled;
  }

  QtObject {
    id: internal
    property bool enabled: false
  }

  IpcHandler {
    target: "idle"

    function toggle(): void {
      root.toggle();
    }
  }

  // The inhibitor holds until its stdin closes, which happens whenever it stops or the shell exits.
  Process {
    running: internal.enabled
    stdinEnabled: true
    command: ["sh", "-c", Config.commands.keepAwake]
    onExited: internal.enabled = false
  }
}
