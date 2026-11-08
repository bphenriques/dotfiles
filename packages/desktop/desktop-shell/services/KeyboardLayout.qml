pragma Singleton

import QtQuick
import Quickshell
import qs

// niri's keyboard layouts, with the short labels from Config.keyboard.labels.
Singleton {
  readonly property var names: Niri.layouts
  readonly property int index: Niri.layoutIndex
  readonly property string name: names[index] ?? ""
  // "" without a label.
  readonly property string label: Config.keyboard.labels[index] ?? ""
}
