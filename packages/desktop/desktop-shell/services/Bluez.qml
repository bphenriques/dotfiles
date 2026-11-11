pragma Singleton

import QtQuick
import Quickshell
import Quickshell.Bluetooth

// Named after the BlueZ daemon so it does not shadow Quickshell's Bluetooth singleton.
Singleton {
  readonly property bool present: Bluetooth.defaultAdapter !== null
  readonly property bool enabled: Bluetooth.defaultAdapter?.enabled ?? false
  readonly property var paired: Array.from(Bluetooth.defaultAdapter?.devices.values ?? []).filter(device => device.paired)
  readonly property var connected: paired.filter(device => device.connected)
}
