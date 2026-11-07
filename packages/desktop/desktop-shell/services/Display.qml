pragma Singleton

import QtQuick
import Quickshell
import Quickshell.Io

// kanshi emits no change events, so the profile is read when the first subscriber appears.
Singleton {
  id: root

  property int subscribers: 0
  property string profile: ""

  onSubscribersChanged: if (subscribers === 1) status.running = true

  Process {
    id: status
    command: ["kanshictl", "status"]
    stdout: StdioCollector {
      onStreamFinished: root.profile = text.match(/Current profile: (.+)/)?.[1] ?? ""
    }
  }
}
