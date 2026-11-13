import QtQuick
import Quickshell
import Quickshell.Io
import qs
import qs.ui

// The wallpaper last set, by name.
// settings.file: under $XDG_RUNTIME_DIR, the path of the wallpaper last set, rewritten on every change.
Integration {
  id: root

  property string path: ""

  glyph: Glyphs.wallpaper
  text: path.replace(/^.*\//, "").replace(/\.[^.]+$/, "")

  FileView {
    path: root.settings.file ? Quickshell.env("XDG_RUNTIME_DIR") + "/" + root.settings.file : ""
    watchChanges: true
    printErrors: false
    onFileChanged: reload()
    onLoaded: root.path = text().trim()
    onLoadFailed: root.path = ""
  }
}
