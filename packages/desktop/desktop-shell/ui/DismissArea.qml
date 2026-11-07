import QtQuick
import Quickshell
import Quickshell.Wayland

// Invisible fullscreen layer under a popup that reports clicks outside it.
// A separate window, so the popup's blur and shadow layer rules stay popup-sized.
PanelWindow {
  id: root

  signal dismissed

  anchors {
    top: true
    bottom: true
    left: true
    right: true
  }
  exclusionMode: ExclusionMode.Ignore
  WlrLayershell.layer: WlrLayer.Top
  WlrLayershell.namespace: "dismiss-area"
  color: "transparent"

  MouseArea {
    anchors.fill: parent
    onClicked: root.dismissed()
  }
}
