pragma Singleton

import QtQuick
import Quickshell
import Quickshell.Io
import qs

// Whether notifications are paused, as Config.commands.notifications prints it: once, then on every change.
Singleton {
  id: root

  readonly property bool enabled: internal.enabled

  function toggle() {
    if (Config.commands.notificationsToggle) Quickshell.execDetached(["sh", "-c", Config.commands.notificationsToggle]);
  }

  QtObject {
    id: internal

    property bool enabled: false

    // Started here rather than by bindings: a running binding can turn true before the command binding has the command.
    function watch() {
      if (!Config.commands.notifications) return;
      watcher.command = ["sh", "-c", Config.commands.notifications];
      watcher.running = true;
    }
  }

  Component.onCompleted: internal.watch()

  Connections {
    target: Config.commands
    function onNotificationsChanged() { internal.watch(); }
  }

  Process {
    id: watcher
    stdout: SplitParser {
      onRead: line => internal.enabled = line.trim() === "true"
    }
    // Delayed so a command that fails at once cannot loop.
    onExited: restartTimer.restart()
  }

  Timer {
    id: restartTimer
    interval: 5000
    onTriggered: internal.watch()
  }
}
