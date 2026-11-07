pragma Singleton

import QtQuick
import Quickshell
import Quickshell.Io

// dunst's paused state: read once, then followed through the PropertiesChanged signal dunst emits on every change.
Singleton {
  id: root

  property bool enabled: false

  QtObject {
    id: parser
    property bool pausedNext: false
  }

  Process {
    running: true
    command: ["dunstctl", "is-paused"]
    stdout: StdioCollector {
      onStreamFinished: root.enabled = text.trim() === "true"
    }
  }

  // dbus-monitor prints the property name and its value on consecutive lines.
  Process {
    running: true
    command: ["dbus-monitor", "--session", "type='signal',path='/org/freedesktop/Notifications',interface='org.freedesktop.DBus.Properties',member='PropertiesChanged'"]
    stdout: SplitParser {
      onRead: line => {
        if (parser.pausedNext) root.enabled = line.includes("boolean true");
        parser.pausedNext = line.includes('string "paused"');
      }
    }
  }
}
