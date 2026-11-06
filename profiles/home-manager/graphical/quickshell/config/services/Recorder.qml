pragma Singleton

import QtQuick
import Quickshell
import Quickshell.Io
import Quickshell.Services.Pipewire

// Contract with packages/desktop/screen-recorder: state file holds the start time (epoch seconds) while recording.
Singleton {
  id: root

  property bool recording: false
  property int startedAt: 0
  property int now: 0
  readonly property int elapsed: Math.max(0, now - startedAt)
  // The recording mix includes the default mic whenever it is unmuted.
  readonly property bool micLive: !(Pipewire.defaultAudioSource?.audio?.muted ?? true)

  PwObjectTracker {
    objects: [Pipewire.defaultAudioSource]
  }

  function stop() {
    stopProcess.startDetached();
  }

  FileView {
    id: state
    path: Quickshell.env("XDG_RUNTIME_DIR") + "/screen-recorder"
    printErrors: false
    onLoaded: {
      root.startedAt = parseInt(text());
      root.recording = true;
    }
    onLoadFailed: root.recording = false
  }

  // FileView cannot watch a file that does not exist yet, so poll.
  Timer {
    interval: 1000
    running: true
    repeat: true
    triggeredOnStart: true
    onTriggered: {
      state.reload();
      root.now = Math.floor(Date.now() / 1000);
    }
  }

  Process {
    id: stopProcess
    command: ["screen-recorder", "stop"]
  }
}
