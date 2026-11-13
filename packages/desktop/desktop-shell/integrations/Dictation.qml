pragma ComponentBehavior: Bound

import QtQuick
import qs
import qs.services
import qs.ui

// A discreet pill on every screen while dictating, under the screen recording one: its icon while it listens, a spinner
// while it transcribes; a click stops it.
// settings.status: prints the dictation's state as it changes, one per line: `recording`, `transcribing`, or anything
// else for idle. settings.stop: the command that stops dictating. settings.glyph: the pill's icon.
Integration {
  id: root

  property string phase: ""
  readonly property bool recording: phase === "recording"
  readonly property bool transcribing: phase === "transcribing"

  glyph: settings.glyph ?? ""
  active: recording || transcribing

  CommandWatcher {
    shell: root.settings.status ?? ""
    onRead: line => root.phase = line.trim()
    onExited: root.phase = ""
  }

  CornerPill {
    shown: root.active
    slot: 1
    // Status, not an alert: it was just started and stops when told.
    tone: "dim"
    namespace: "dictation-indicator"
    onClicked: root.exec(root.settings.stop)

    Item {
      implicitWidth: Config.theme.iconSize
      implicitHeight: Config.theme.iconSize

      Icon {
        anchors.fill: parent
        visible: !root.transcribing
        tone: Audio.sourceMuted ? "dim" : "normal"
        glyph: root.glyph
      }

      Spinner {
        anchors.fill: parent
        visible: root.transcribing
        tone: "normal"
      }
    }
  }
}
