pragma Singleton

import QtQuick
import Quickshell
import Quickshell.Bluetooth

// Named after the BlueZ daemon so it does not shadow Quickshell's Bluetooth singleton.
Singleton {
  readonly property var adapter: Bluetooth.defaultAdapter
  readonly property bool present: adapter !== null
  readonly property bool enabled: adapter?.enabled ?? false
  readonly property var paired: Array.from(adapter?.devices.values ?? []).filter(device => device.paired)
  readonly property var connected: paired.filter(device => device.connected)
}
