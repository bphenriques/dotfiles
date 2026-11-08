pragma Singleton

import QtQuick
import Quickshell
import Quickshell.Services.Pipewire
import qs

// Default output and input.
Singleton {
  id: root

  readonly property PwNode sink: Pipewire.defaultAudioSink
  readonly property PwNode source: Pipewire.defaultAudioSource

  // The output's type (headset, headphones, internal, external) and friendly name, from Config.commands.audioDevice.
  property string sinkType: ""
  property string sinkLabel: ""

  readonly property string sinkName: sinkLabel || sink?.nickname || sink?.description || ""
  readonly property real sinkVolume: sink?.audio?.volume ?? 0
  readonly property real sourceVolume: source?.audio?.volume ?? 0
  readonly property bool sinkMuted: sink?.audio?.muted ?? false
  readonly property string sourceName: source?.nickname || source?.description || ""
  readonly property string sourceType: sources.find(device => device.name === source?.name)?.type ?? "microphone"
  readonly property bool sourceMuted: source?.audio?.muted ?? false
  // Set while the menu is open: the device list only follows it then.
  property bool live: false
  // Set while the input's level is shown: it is only sampled then.
  property bool metering: false
  // On the same linear scale as the volume.
  readonly property real sourceLevel: Math.min(1, sourcePeak.peak)

  // Every output and input as the tool lists them, internal first: { kind, name, type, label }.
  property var devices: []
  readonly property var sinks: devices.filter(device => device.kind === "sink")
  readonly property var sources: devices.filter(device => device.kind === "source")

  function refreshDevices() {
    lister.refresh();
  }

  function select(kind, name) {
    if (Config.commands.audioSelect) Quickshell.execDetached(["sh", "-c", Config.commands.audioSelect + ' "$1" "$2"', "sh", kind, name]);
  }

  // Re-classifies the output; the menu also asks on open, to catch headphones plugged into the same output.
  function refresh() {
    classifier.refresh();
  }

  Connections {
    target: Pipewire.nodes
    enabled: root.live
    function onValuesChanged() { root.refreshDevices(); }
  }

  PwObjectTracker {
    objects: [root.sink, root.source]
  }

  PwNodePeakMonitor {
    id: sourcePeak
    node: root.source
    enabled: root.metering && !root.sourceMuted
  }

  Connections {
    target: Pipewire
    function onDefaultAudioSourceChanged() { root.refreshDevices(); }

    function onDefaultAudioSinkChanged() {
      // What the device list knows shows at once, so the old device's glyph does not linger until the classifier answers.
      const known = root.devices.find(device => device.name === Pipewire.defaultAudioSink?.name);
      if (known) {
        root.sinkType = known.type;
        root.sinkLabel = known.label;
      }
      root.refresh();
    }
  }

  CommandReader {
    id: lister
    shell: Config.commands.audioDevices
    atStart: true
    onRead: text => root.devices = text.trim().split("\n").filter(line => line).map(line => {
      const [kind, name, type, label] = line.split("\t");
      return { kind, name, type, label };
    })
  }

  CommandReader {
    id: classifier
    shell: Config.commands.audioDevice
    atStart: true
    onRead: text => {
      const [type, label] = text.trim().split("\t");
      root.sinkType = type ?? "";
      root.sinkLabel = label ?? "";
    }
  }
}
