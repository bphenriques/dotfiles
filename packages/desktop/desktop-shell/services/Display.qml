pragma Singleton

import QtQuick
import Quickshell
import qs

// The display profile and the outputs, read on refresh(): neither emits change events the shell can follow.
Singleton {
  id: root

  property string profile: ""
  // Each output twice, as profiles match them: its connector name, and its make, model and serial.
  property var connected: []
  // The same, for the outputs that are on.
  property var enabled: []

  function refresh() {
    status.refresh();
    outputs.refresh();
  }

  // Whether an entry's requirements hold: the outputs it needs connected, those it needs off, and
  // those it needs on.
  function available(requires) {
    return Array.from(requires?.connected ?? []).every(output => root.connected.includes(output))
      && Array.from(requires?.disconnected ?? []).every(output => !root.connected.includes(output))
      && Array.from(requires?.enabled ?? []).every(output => root.enabled.includes(output));
  }

  function names(name, output) {
    return [name, `${output.make} ${output.model} ${output.serial ?? "Unknown"}`];
  }

  CommandReader {
    id: status
    shell: Config.commands.displayProfile
    onRead: text => root.profile = text.trim()
  }

  // niri reports an output that is off without a logical size.
  CommandReader {
    id: outputs
    shell: Config.commands.outputs
    onRead: text => {
      const all = Object.entries(JSON.parse(text));
      root.connected = [].concat(...all.map(([name, output]) => root.names(name, output)));
      root.enabled = [].concat(...all.filter(([, output]) => output.logical).map(([name, output]) => root.names(name, output)));
    }
  }
}
