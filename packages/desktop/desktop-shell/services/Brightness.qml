pragma Singleton

import QtQuick
import Quickshell
import Quickshell.Io

// The backlight brightnessctl controls. Its sysfs file is watched: every brightnessctl write notifies it.
Singleton {
  id: root

  property string device: ""
  property int maximum: 0
  property int current: 0
  readonly property bool present: maximum > 0
  readonly property int percentage: present ? Math.round(current * 100 / maximum) : 0

  Process {
    running: true
    command: ["brightnessctl", "--machine-readable", "--class=backlight", "info"]
    stdout: StdioCollector {
      onStreamFinished: {
        const [name, , value, , maximum] = text.trim().split(",");
        root.device = name ?? "";
        root.current = parseInt(value) || 0;
        root.maximum = parseInt(maximum) || 0;
      }
    }
  }

  FileView {
    path: root.device ? "/sys/class/backlight/" + root.device + "/brightness" : ""
    watchChanges: true
    printErrors: false
    onFileChanged: reload()
    onLoaded: root.current = parseInt(text()) || 0
  }
}
