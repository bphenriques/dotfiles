pragma Singleton

import QtQuick
import Quickshell
import Quickshell.Io

// Contract with packages/desktop/awww-util: the state file holds the path of the wallpaper last set.
Singleton {
  id: root

  property string path: ""
  readonly property string name: path.replace(/^.*\//, "").replace(/\.[^.]+$/, "")

  FileView {
    path: Quickshell.env("XDG_RUNTIME_DIR") + "/wallpaper"
    watchChanges: true
    printErrors: false
    onFileChanged: reload()
    onLoaded: root.path = text().trim()
    onLoadFailed: root.path = ""
  }
}
