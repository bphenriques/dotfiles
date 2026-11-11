import QtQuick
import Quickshell.Wayland

// Invisible full-output layer under a popup that reports clicks outside it; one per screen, so a click on any screen counts.
OverlayWindow {
  id: root

  signal dismissed

  anchors {
    top: true
    bottom: true
    left: true
    right: true
  }
  WlrLayershell.namespace: "dismiss-area"

  MouseArea {
    anchors.fill: parent
    onClicked: root.dismissed()
  }
}
