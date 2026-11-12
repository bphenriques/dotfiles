pragma ComponentBehavior: Bound

import QtQuick
import Quickshell
import Quickshell.Io
import Quickshell.Wayland
import qs
import qs.services
import qs.ui

// A pill on every screen while a screen recording runs: its time and whether the mic is in it; a click stops it.
// settings.file: under $XDG_RUNTIME_DIR, the start time in epoch seconds while recording, empty otherwise.
// settings.stop: the command that stops the recording.
Integration {
  id: root

  property int startedAt: 0
  property int now: 0
  readonly property bool recording: startedAt > 0
  readonly property int elapsed: Math.max(0, now - startedAt)
  // The recording mix includes the default mic whenever it is unmuted.
  readonly property bool micLive: !Audio.sourceMuted

  glyph: Glyphs.record
  text: recording ? Math.floor(elapsed / 60) + ":" + String(elapsed % 60).padStart(2, "0") : ""
  active: recording

  // The time the pill shows, kept while it fades out so it holds its size.
  property string shownTime: ""
  onTextChanged: if (recording) shownTime = text

  Fade {
    id: fade
    shown: root.recording
  }

  function stop() {
    if (settings.stop) Quickshell.execDetached(["sh", "-c", settings.stop]);
  }

  FileView {
    path: root.settings.file ? Quickshell.env("XDG_RUNTIME_DIR") + "/" + root.settings.file : ""
    watchChanges: true
    printErrors: false
    onFileChanged: reload()
    onLoaded: root.startedAt = parseInt(text()) || 0
    onLoadFailed: root.startedAt = 0
  }

  Timer {
    interval: 1000
    running: root.recording
    repeat: true
    triggeredOnStart: true
    onTriggered: root.now = Math.floor(Date.now() / 1000)
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
        margins.top: Config.theme.edgeMargin
        margins.right: Config.theme.edgeMargin
        WlrLayershell.namespace: "recording-indicator"

        implicitWidth: pill.implicitWidth
        implicitHeight: pill.implicitHeight

        Pill {
          id: pill
          opacity: fade.value
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
            anchors.verticalCenter: parent.verticalCenter
            text: mouse.containsMouse ? "Stop".padEnd(root.shownTime.length) : root.shownTime
          }

          Icon {
            anchors.verticalCenter: parent.verticalCenter
            tone: root.micLive ? "alert" : "dim"
            glyph: root.micLive ? Glyphs.microphone : Glyphs.microphoneOff
          }
        }

        MouseArea {
          id: mouse
          anchors.fill: pill
          hoverEnabled: true
          onClicked: root.stop()
        }
      }
    }
  }
}
