import QtQuick
import qs

// A rounded capsule around a row of content, outlined in its tone's colour.
Surface {
  id: root

  property string tone: "primary"
  default property alias content: row.data

  implicitWidth: row.implicitWidth + 2 * Config.theme.spacing
  implicitHeight: row.implicitHeight + Config.theme.spacing
  radius: height / 2
  border.color: tone === "alert" ? Config.theme.alert : tone === "warning" ? Config.theme.warning : Config.theme.primary
  border.width: 2 * Config.theme.borderWidth

  Row {
    id: row
    anchors.centerIn: parent
    spacing: Config.theme.spacing
  }
}
