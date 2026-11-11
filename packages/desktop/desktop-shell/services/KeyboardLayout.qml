pragma Singleton

import QtQuick
import Quickshell
import qs

// The compositor's keyboard layouts, with the short labels from Config.keyboard.labels.
Singleton {
  readonly property var names: Compositor.layouts
  readonly property int index: Compositor.layoutIndex
  readonly property string name: names[index] ?? ""
  // "" without a label.
  readonly property string label: names.length ? Config.keyboard.labels[index] ?? "" : ""

  function next() {
    Compositor.nextLayout();
  }
}
