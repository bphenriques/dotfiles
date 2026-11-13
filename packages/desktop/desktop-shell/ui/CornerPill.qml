pragma ComponentBehavior: Bound

import QtQuick
import Quickshell
import Quickshell.Wayland
import qs

// A pill in the top right corner of every screen while `shown`, fading in and out; `slot` stacks it under the ones above.
Scope {
  id: root

  property bool shown: false
  property int slot: 0
  property string tone: "primary"
  property string namespace
  default property Component content
  readonly property bool hovered: internal.hovered

  signal clicked

  QtObject {
    id: internal

    property bool hovered: false
  }

  Fade {
    id: fade
    shown: root.shown
  }

  LazyLoader {
    active: fade.loaded

    Variants {
      model: Quickshell.screens

      OverlayWindow {
        required property var modelData
        screen: modelData

        anchors.top: true
        anchors.right: true
        margins.top: Config.theme.edgeMargin + root.slot * (implicitHeight + Config.theme.spacing)
        margins.right: Config.theme.edgeMargin
        WlrLayershell.namespace: root.namespace

        implicitWidth: pill.implicitWidth
        implicitHeight: pill.implicitHeight

        Pill {
          id: pill
          tone: root.tone
          opacity: fade.value
          anchors.fill: parent

          Loader {
            anchors.verticalCenter: parent.verticalCenter
            sourceComponent: root.content
          }
        }

        MouseArea {
          anchors.fill: pill
          hoverEnabled: true
          onContainsMouseChanged: internal.hovered = containsMouse
          onClicked: root.clicked()
        }
      }
    }
  }
}
