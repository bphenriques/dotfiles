import QtQuick
import QtQuick.Layouts
import qs

// Row with pointer input and a highlight under the pointer or while current; children are laid out in a RowLayout spanning its width.
Rectangle {
  id: root

  default property alias content: row.data
  property bool dimsWhenDisabled: true
  // The item the keys are on (a selected day, an event in focus), highlighted as the pointer does.
  property bool current: false
  property real verticalPadding: Config.theme.itemPaddingY

  signal activated
  signal secondaryActivated
  signal scrolled(bool up)

  implicitWidth: row.implicitWidth + 2 * Config.theme.itemPaddingX
  // Squeezed in a layout, no narrower than its content can go (its text shortened, never its icons).
  Layout.minimumWidth: row.Layout.minimumWidth + 2 * Config.theme.itemPaddingX
  implicitHeight: row.implicitHeight + 2 * verticalPadding
  radius: Config.theme.itemRadius
  // Disabled rows dim whole, images included.
  opacity: enabled || !dimsWhenDisabled ? 1 : Config.theme.dimOpacity
  // Highlighted under the pointer or while current, more so while pressed, so a click is felt.
  color: mouse.pressed ? Tone.wash("primary", 2) : current || mouse.containsMouse ? Tone.wash("primary") : "transparent"
  Behavior on color {
    ColorAnimation {
      duration: Config.theme.animationDuration
    }
  }

  RowLayout {
    id: row
    anchors.verticalCenter: parent.verticalCenter
    anchors.left: parent.left
    anchors.right: parent.right
    anchors.leftMargin: Config.theme.itemPaddingX
    anchors.rightMargin: Config.theme.itemPaddingX
    spacing: Config.theme.spacing
  }

  WheelSteps {
    id: mouse
    anchors.fill: parent
    hoverEnabled: true
    acceptedButtons: Qt.LeftButton | Qt.RightButton
    onClicked: event => event.button === Qt.RightButton ? root.secondaryActivated() : root.activated()
    onStepped: up => root.scrolled(up)
  }
}
