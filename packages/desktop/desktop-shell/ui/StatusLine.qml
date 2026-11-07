import QtQuick
import QtQuick.Layouts
import qs

// Icon, text cut with an ellipsis when it does not fit, and an optional dim detail on the right.
// `tone` colours the icon and text (see Label).
RowLayout {
  id: root

  property string glyph
  property string image
  property string text
  property string detail
  property string tone: "normal"

  spacing: Config.theme.spacing

  Icon {
    glyph: root.glyph
    source: root.image
    tone: root.tone
  }

  Label {
    visible: root.text !== ""
    Layout.fillWidth: true
    elide: Text.ElideRight
    tone: root.tone
    text: root.text
  }

  Label {
    visible: root.detail !== ""
    tone: "dim"
    text: root.detail
  }
}
