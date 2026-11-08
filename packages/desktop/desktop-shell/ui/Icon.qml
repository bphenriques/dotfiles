import QtQuick
import Quickshell.Widgets
import qs

// A font glyph or an image (e.g. a tray app's icon) in the same square, so rows line up either way.
Item {
  id: root

  property string glyph: ""
  property string source: ""
  property string tone: "normal"
  property real size: Config.theme.iconSize

  visible: glyph !== "" || source !== ""
  implicitWidth: size
  implicitHeight: size

  Label {
    id: glyphLabel
    anchors.centerIn: parent
    visible: root.source === ""
    tone: root.tone
    // A point is 4/3 of a pixel, so the glyph fills the square. Bindings can read the size before it is set; Qt rejects a zero point size.
    font.pointSize: Math.max(1, root.size * 0.75)
    text: root.glyph
  }

  IconImage {
    anchors.fill: parent
    visible: root.source !== ""
    source: root.source
    // An image has no colour to dim, so a dim one fades instead.
    opacity: root.tone === "dim" ? Config.theme.dimOpacity : 1
  }
}
