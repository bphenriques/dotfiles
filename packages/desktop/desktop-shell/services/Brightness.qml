pragma Singleton

import QtQuick
import Quickshell
import Quickshell.Io
import qs

// The backlight's file and maximum come from Config.commands.brightness; the file is watched, every write to it notifies.
Singleton {
  id: root

  property string file: ""
  property int maximum: 0
  property int current: 0
  readonly property bool present: maximum > 0
  readonly property int percentage: present ? Math.round(current * 100 / maximum) : 0

  CommandReader {
    shell: Config.commands.brightness
    atStart: true
    onRead: text => {
      const [file, maximum] = text.trim().split("\t");
      root.file = file ?? "";
      root.maximum = parseInt(maximum) || 0;
    }
  }

  FileView {
    path: root.file
    watchChanges: true
    printErrors: false
    onFileChanged: reload()
    onLoaded: root.current = parseInt(text()) || 0
  }
}
