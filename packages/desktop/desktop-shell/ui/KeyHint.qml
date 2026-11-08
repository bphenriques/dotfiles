import QtQuick
import qs
import "WhichKey.js" as WhichKey

// The key that triggers an item, in a gutter of `chars` characters so a level's labels start together.
Item {
  id: root

  property var key
  property bool paired: false
  property int chars: 1

  implicitWidth: cell.advanceWidth * chars
  implicitHeight: label.implicitHeight

  TextMetrics {
    id: cell
    font: label.font
    text: "0"
  }

  Label {
    id: label
    tone: "primary"
    font.bold: true
    text: WhichKey.label(root.key, root.paired)
  }
}
