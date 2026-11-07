import QtQuick
import QtQuick.Layouts
import qs

// Row with pointer input and a hover highlight; children are laid out in a RowLayout spanning its width.
Rectangle {
  id: root

  property int wheelDelta: 0
  default property alias content: row.data

  signal activated
  signal secondaryActivated
  signal scrolled(bool up)

  implicitWidth: row.implicitWidth + 2 * Config.theme.itemPaddingX
  implicitHeight: row.implicitHeight + 2 * Config.theme.itemPaddingY
  radius: Config.theme.itemRadius
  color: mouse.containsMouse ? Qt.alpha(Config.theme.primary, Config.theme.highlightOpacity) : "transparent"

  RowLayout {
    id: row
    anchors.verticalCenter: parent.verticalCenter
    anchors.left: parent.left
    anchors.right: parent.right
    anchors.leftMargin: Config.theme.itemPaddingX
    anchors.rightMargin: Config.theme.itemPaddingX
    spacing: Config.theme.spacing
  }

  MouseArea {
    id: mouse
    anchors.fill: parent
    hoverEnabled: true
    acceptedButtons: Qt.LeftButton | Qt.RightButton
    onClicked: event => event.button === Qt.RightButton ? root.secondaryActivated() : root.activated()
    // One step per wheel notch; touchpads send many small deltas.
    onWheel: wheel => {
      root.wheelDelta += wheel.angleDelta.y;
      while (Math.abs(root.wheelDelta) >= 120) {
        root.scrolled(root.wheelDelta > 0);
        root.wheelDelta -= Math.sign(root.wheelDelta) * 120;
      }
    }
  }
}
