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
  // Characters the detail always takes, so what is beside it holds still as the value changes.
  property int detailChars: 0
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

  TextMetrics {
    id: cell
    font: detailLabel.font
    text: "0"
  }

  Label {
    id: detailLabel
    visible: root.detail !== ""
    Layout.minimumWidth: root.detailChars * cell.advanceWidth
    horizontalAlignment: Text.AlignRight
    tone: "dim"
    text: root.detail
  }
}
