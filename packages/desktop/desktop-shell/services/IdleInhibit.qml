pragma Singleton

import QtQuick
import Quickshell
import Quickshell.Io
import qs

// Keeps the screen awake with Config.commands.keepAwake, a systemd idle inhibitor hypridle honours. Off again when the shell restarts.
Singleton {
  id: root

  property bool enabled: false

  IpcHandler {
    target: "idle"

    function toggle(): void {
      root.enabled = !root.enabled;
    }
  }

  // The inhibitor holds until its stdin closes, which happens whenever it stops or the shell exits.
  Process {
    running: root.enabled
    stdinEnabled: true
    command: ["sh", "-c", Config.commands.keepAwake]
    onExited: root.enabled = false
  }
}
