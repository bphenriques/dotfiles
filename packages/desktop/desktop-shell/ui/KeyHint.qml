import QtQuick
import qs
import "WhichKey.js" as WhichKey

// The key that triggers an item, followed by the separator: the menu's `a ➜`.
Row {
  id: root

  property var key

  spacing: Config.theme.spacing

  Label {
    tone: "primary"
    font.bold: true
    text: WhichKey.label(root.key)
  }

  Label {
    tone: "dim"
    text: Glyphs.separator
  }
}
