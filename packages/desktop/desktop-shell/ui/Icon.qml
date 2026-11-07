import QtQuick
import Quickshell.Widgets
import qs

// A font glyph or an image (e.g. a tray app's icon) in the same square, so rows line up either way.
Item {
  id: root

  property string glyph: ""
  property string source: ""
  property alias tone: glyphLabel.tone

  visible: glyph !== "" || source !== ""
  implicitWidth: Config.theme.iconSize
  implicitHeight: Config.theme.iconSize

  Label {
    id: glyphLabel
    anchors.centerIn: parent
    visible: root.source === ""
    text: root.glyph
  }

  IconImage {
    anchors.fill: parent
    visible: root.source !== ""
    source: root.source
  }
}
