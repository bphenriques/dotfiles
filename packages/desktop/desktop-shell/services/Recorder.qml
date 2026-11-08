pragma Singleton

import QtQuick
import Quickshell
import Quickshell.Io
import qs

// Contract with packages/desktop/screen-recorder: the state file holds the start time (epoch seconds) while recording and is empty otherwise.
Singleton {
  id: root

  property int startedAt: 0
  property int now: 0
  readonly property bool recording: startedAt > 0
  readonly property int elapsed: Math.max(0, now - startedAt)
  // The recording mix includes the default mic whenever it is unmuted.
  readonly property bool micLive: !Audio.sourceMuted

  function stop() {
    if (Config.commands.recordingStop) Quickshell.execDetached(["sh", "-c", Config.commands.recordingStop]);
  }

  FileView {
    path: Quickshell.env("XDG_RUNTIME_DIR") + "/screen-recorder"
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
}
