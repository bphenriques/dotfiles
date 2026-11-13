pragma Singleton

import QtQuick
import Quickshell
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
  }

  CommandWatcher {
    shell: Config.notifications.watch ?? ""
    onRead: line => internal.enabled = line.trim() === "true"
  }
}
