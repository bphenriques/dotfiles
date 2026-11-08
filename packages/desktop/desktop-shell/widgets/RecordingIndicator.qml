pragma ComponentBehavior: Bound

import QtQuick
import Quickshell
import Quickshell.Wayland
import qs
import qs.services
import qs.ui

LazyLoader {
  active: Recorder.recording

  Variants {
    model: Quickshell.screens

    OverlayWindow {
      required property var modelData
      screen: modelData

      anchors.top: true
      anchors.right: true
      margins.top: Config.theme.edgeMargin
      margins.right: Config.theme.edgeMargin
      WlrLayershell.namespace: "recording-indicator"

      implicitWidth: pill.implicitWidth
      implicitHeight: pill.implicitHeight

      Pill {
        id: pill
        anchors.fill: parent
        tone: "alert"

        // The record dot; it squares into a stop button on hover.
        Rectangle {
          anchors.verticalCenter: parent.verticalCenter
          width: Config.theme.iconSize / 3
          height: width
          radius: mouse.containsMouse ? Config.theme.borderWidth * 2 : width / 2
          color: Tone.color("alert")
        }

        // Padded to the time's width (the font is monospace), so the pill keeps its size under the pointer.
        Label {
          readonly property string time: Math.floor(Recorder.elapsed / 60) + ":" + String(Recorder.elapsed % 60).padStart(2, "0")

          anchors.verticalCenter: parent.verticalCenter
          text: mouse.containsMouse ? "Stop".padEnd(time.length) : time
        }

        Icon {
          anchors.verticalCenter: parent.verticalCenter
          tone: Recorder.micLive ? "alert" : "dim"
          glyph: Recorder.micLive ? Glyphs.microphone : Glyphs.microphoneOff
        }
      }

      MouseArea {
        id: mouse
        anchors.fill: pill
        hoverEnabled: true
        cursorShape: Qt.PointingHandCursor
        onClicked: Recorder.stop()
      }
    }
  }
}
