pragma Singleton

import QtQuick
import Quickshell
import qs

// The theme font's measures, so sizes given in characters or lines agree everywhere.
Singleton {
  // A character's width, the font being monospace.
  readonly property real cell: cellMetrics.advanceWidth
  // A line's height, as a Label draws one.
  readonly property real line: fontMetrics.height

  TextMetrics {
    id: cellMetrics
    font: fontMetrics.font
    text: "0"
  }

  FontMetrics {
    id: fontMetrics
    font.family: Config.theme.monospaceFont
    font.pointSize: Config.theme.fontSize
  }
}
