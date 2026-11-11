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

  // Every output and input as Config.commands.audioDevices lists them, internal first: { kind, name, type, label }.
  readonly property var sinks: internal.devices.filter(device => device.kind === "sink")
  readonly property var sources: internal.devices.filter(device => device.kind === "source")

  // The type (headset, headphones, internal, external, microphone) picks the glyph.
  readonly property string sinkType: internal.sinkDevice?.type ?? ""
  readonly property string sourceType: internal.sourceDevice?.type ?? "microphone"
  readonly property string sinkName: internal.sinkDevice?.label || sink?.nickname || sink?.description || ""
  readonly property string sourceName: internal.sourceDevice?.label || source?.nickname || source?.description || ""
  readonly property real sinkVolume: sink?.audio?.volume ?? 0
  readonly property real sourceVolume: source?.audio?.volume ?? 0
  readonly property bool sinkMuted: sink?.audio?.muted ?? false
  readonly property bool sourceMuted: source?.audio?.muted ?? false
  // Set while the menu is open: the device list only follows it then.
  property bool live: false
  // Set while the input's level is shown: it is only sampled then.
  property bool metering: false
  // On the same linear scale as the volume.
  readonly property real sourceLevel: Math.min(1, sourcePeak.peak)

  // The menu also asks on open, to catch headphones plugged into the same output.
  function refreshDevices() {
    lister.refresh();
  }

  function select(kind, name) {
    if (Config.commands.audioSelect) Quickshell.execDetached(["sh", "-c", Config.commands.audioSelect + ' "$1" "$2"', "sh", kind, name]);
  }

  QtObject {
    id: internal
    property var devices: []
    readonly property var sinkDevice: root.sinks.find(device => device.name === root.sink?.name) ?? null
    readonly property var sourceDevice: root.sources.find(device => device.name === root.source?.name) ?? null
  }

  Connections {
    target: Pipewire.nodes
    enabled: root.live
    function onValuesChanged() { root.refreshDevices(); }
  }

  Connections {
    target: Pipewire
    function onDefaultAudioSinkChanged() { root.refreshDevices(); }
    function onDefaultAudioSourceChanged() { root.refreshDevices(); }
  }

  PwObjectTracker {
    objects: [root.sink, root.source]
  }

  PwNodePeakMonitor {
    id: sourcePeak
    node: root.source
    enabled: root.metering && !root.sourceMuted
  }

  CommandReader {
    id: lister
    shell: Config.commands.audioDevices
    atStart: true
    onRead: text => internal.devices = text.trim().split("\n").filter(line => line).map(line => {
      const [kind, name, type, label] = line.split("\t");
      return { kind, name, type, label };
    })
  }
}
