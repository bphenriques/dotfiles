pragma Singleton

import QtQuick
import Quickshell
import Quickshell.Io
import qs

// Whether notifications are paused, as Config.notifications.watch prints it: once, then on every change.
Singleton {
  id: root

  readonly property bool enabled: internal.enabled

  function toggle() {
    if (Config.notifications.toggle) Quickshell.execDetached(["sh", "-c", Config.notifications.toggle]);
  }

  QtObject {
    id: internal

    property bool enabled: false

    // Started here rather than by bindings: a running binding can turn true before the command binding has the command.
    function watch() {
      if (!Config.notifications.watch) return;
      watcher.command = ["sh", "-c", Config.notifications.watch];
      watcher.running = true;
    }
  }

  Component.onCompleted: internal.watch()

  Connections {
    target: Config.notifications
    function onWatchChanged() { internal.watch(); }
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
