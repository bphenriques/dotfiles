import QtQuick
import qs

// A thin bar filled to `value` (0 to 1, clamped) in its tone's colour, from the left or, vertical, from the bottom.
Rectangle {
  id: root

  property real value: 0
  property string tone: "primary"
  property int orientation: Qt.Horizontal

  readonly property bool vertical: orientation === Qt.Vertical
  readonly property real filled: Math.max(0, Math.min(1, value))

  implicitWidth: vertical ? Config.theme.meterThickness : 0
  implicitHeight: vertical ? 0 : Config.theme.meterThickness
  radius: Math.min(width, height) / 2
  color: Qt.alpha(Config.theme.foreground, Config.theme.highlightOpacity)

  Rectangle {
    anchors.left: parent.left
    anchors.bottom: parent.bottom
    width: root.vertical ? parent.width : parent.width * root.filled
    height: root.vertical ? parent.height * root.filled : parent.height
    radius: parent.radius
    color: Tone.color(root.tone)

    Behavior on width {
      NumberAnimation {
        duration: 80
      }
    }
    Behavior on height {
      NumberAnimation {
        duration: 80
      }
    }
  }
}
