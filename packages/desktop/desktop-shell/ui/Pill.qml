import QtQuick
import qs

// A rounded capsule around a row of content, outlined in its tone's colour.
Surface {
  id: root

  property string tone: "primary"
  property real padding: Config.theme.spacing
  default property alias content: row.data

  implicitWidth: row.implicitWidth + 2 * padding
  implicitHeight: row.implicitHeight + padding
  radius: height / 2
  border.color: Tone.color(tone)
  border.width: Config.theme.borderWidth

  Row {
    id: row
    anchors.centerIn: parent
    spacing: Config.theme.spacing
  }
}
