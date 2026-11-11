pragma Singleton

import QtQuick
import Quickshell

// Values arriving in the first moments are the current state replayed, not changes worth showing.
Singleton {

  readonly property bool settled: !timer.running

  Timer {
    id: timer
    running: true
    interval: 2000
  }
}
