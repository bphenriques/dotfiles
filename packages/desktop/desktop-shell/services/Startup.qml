pragma Singleton

import QtQuick
import Quickshell

// Values arriving in the first moments are the current state replayed, not changes worth showing.
Singleton {
  id: root

  property bool settled: false

  Timer {
    running: true
    interval: 2000
    onTriggered: root.settled = true
  }
}
