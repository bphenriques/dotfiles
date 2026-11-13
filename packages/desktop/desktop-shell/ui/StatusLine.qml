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
  // Glyphs on the right before the detail (a mode, a submenu), drawn as icons the height of its text.
  property var detailGlyphs: []
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
    // Squeezed, text ends in an ellipsis, but a short value (80%, EN) stays whole.
    Layout.minimumWidth: Math.min(implicitWidth, 6 * Metrics.cell)
    elide: Text.ElideRight
    tone: root.tone
    text: root.text
  }

  Repeater {
    model: root.detailGlyphs

    Icon {
      required property string modelData
      size: detailLabel.implicitHeight
      tone: "dim"
      glyph: modelData
    }
  }

  Label {
    id: detailLabel
    visible: root.detail !== ""
    Layout.minimumWidth: Math.max(implicitWidth, root.detailChars * Metrics.cell)
    horizontalAlignment: Text.AlignRight
    tone: "dim"
    text: root.detail
  }
}
