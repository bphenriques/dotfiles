import QtQuick
import Quickshell
import Quickshell.Wayland
import qs
import qs.services

Variants {
  model: Quickshell.screens

  PanelWindow {
    required property var modelData
    screen: modelData
    visible: Recorder.recording

    anchors.top: true
    anchors.right: true
    margins.top: 12
    margins.right: 12
    exclusionMode: ExclusionMode.Ignore
    WlrLayershell.layer: WlrLayer.Overlay
    WlrLayershell.namespace: "recording-indicator"

    color: "transparent"
    implicitWidth: pill.width
    implicitHeight: pill.height

    Rectangle {
      id: pill
      width: content.implicitWidth + 24
      height: 32
      radius: height / 2
      color: Qt.alpha(Theme.background, 0.9)
      border.color: Theme.accent
      border.width: 2

      Row {
        id: content
        anchors.centerIn: parent
        spacing: 8

        Rectangle {
          anchors.verticalCenter: parent.verticalCenter
          width: 10
          height: 10
          radius: mouse.containsMouse ? 2 : 5
          color: Theme.accent
        }

        Text {
          anchors.verticalCenter: parent.verticalCenter
          color: Theme.foreground
          font.family: Theme.monospaceFont
          text: mouse.containsMouse
            ? "Stop"
            : Math.floor(Recorder.elapsed / 60) + ":" + String(Recorder.elapsed % 60).padStart(2, "0")
        }
      }

      MouseArea {
        id: mouse
        anchors.fill: parent
        hoverEnabled: true
        cursorShape: Qt.PointingHandCursor
        onClicked: Recorder.stop()
      }
    }
  }
}
