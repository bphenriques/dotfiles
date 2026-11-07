pragma Singleton

import QtQuick
import Quickshell
import Quickshell.Io
import Quickshell.Services.Pipewire

// Default output and input. Volume and mute are only current while subscribed.
Singleton {
  id: root

  property int subscribers: 0
  readonly property PwNode sink: Pipewire.defaultAudioSink
  readonly property PwNode source: Pipewire.defaultAudioSource

  // volume-osd's classification of the output (headset, headphones, internal, external) and its friendly name.
  property string sinkType: ""
  property string sinkLabel: ""

  readonly property string sinkName: sinkLabel || sink?.nickname || sink?.description || ""
  readonly property real sinkVolume: sink?.audio?.volume ?? 0
  readonly property real sourceVolume: source?.audio?.volume ?? 0
  readonly property bool sinkMuted: sink?.audio?.muted ?? false
  readonly property string sourceName: source?.nickname || source?.description || ""
  readonly property bool sourceMuted: source?.audio?.muted ?? false

  // A new subscriber also re-classifies, to catch headphones plugged into the same output.
  onSubscribersChanged: if (subscribers === 1) classify()

  // A request while volume-osd is still answering runs it once more afterwards, so the last device wins.
  function classify() {
    if (classifier.running) state.pending = true;
    else classifier.running = true;
  }

  QtObject {
    id: state
    property bool pending: false
  }

  PwObjectTracker {
    objects: root.subscribers > 0 ? [root.sink, root.source] : []
  }

  Connections {
    target: Pipewire
    function onDefaultAudioSinkChanged() {
      root.classify();
    }
  }

  Process {
    id: classifier
    running: true
    command: ["volume-osd", "sink-status"]
    onExited: if (state.pending) {
      state.pending = false;
      running = true;
    }
    stdout: StdioCollector {
      onStreamFinished: {
        const [type, label] = text.trim().split("\t");
        root.sinkType = type ?? "";
        root.sinkLabel = label ?? "";
      }
    }
  }
}
