pragma Singleton

import QtQuick
import Quickshell
import Quickshell.Io
import qs

// Whether notifications are paused, as Config.commands.notifications prints it: once, then on every change.
Singleton {
  id: root

  property bool enabled: false

  // Started here rather than by bindings: a running binding can turn true before the command binding has the command.
  function watch() {
    if (!Config.commands.notifications) return;
    watcher.command = ["sh", "-c", Config.commands.notifications];
    watcher.running = true;
  }

  Component.onCompleted: watch()

  Connections {
    target: Config.commands
    function onNotificationsChanged() { root.watch(); }
  }

  Process {
    id: watcher
    stdout: SplitParser {
      onRead: line => root.enabled = line.trim() === "true"
    }
    // Delayed so a command that fails at once cannot loop.
    onExited: restartTimer.restart()
  }

  Timer {
    id: restartTimer
    interval: 5000
    onTriggered: root.watch()
  }
}
