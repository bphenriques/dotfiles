pragma Singleton

import QtQuick
import Quickshell
import Quickshell.Io
import qs

// The backlight's file and maximum come from Config.commands.brightness; the file is watched, every write to it notifies.
Singleton {
  id: root

  readonly property bool present: internal.maximum > 0
  // The panel it dims is on, as of the last time the menu opened.
  readonly property bool lit: present && Compositor.enabled.includes(Config.display.panel)
  readonly property int percentage: present ? Math.round(internal.current * 100 / internal.maximum) : 0

  QtObject {
    id: internal
    property string file: ""
    property int maximum: 0
    property int current: 0
  }

  CommandReader {
    shell: Config.commands.brightness
    atStart: true
    onRead: text => {
      const [file, maximum] = text.trim().split("\t");
      internal.file = file ?? "";
      internal.maximum = parseInt(maximum) || 0;
    }
  }

  FileView {
    path: internal.file
    watchChanges: true
    printErrors: false
    onFileChanged: reload()
    onLoaded: internal.current = parseInt(text()) || 0
  }
}
