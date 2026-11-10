pragma Singleton

import QtQuick
import Quickshell
import qs

// The outputs, read on refresh(): niri emits no change events the shell can follow.
Singleton {
  id: root

  // Each output twice, as niri can be told about it: its connector name, and its make, model and serial.
  property var connected: []
  // The same, for the outputs that are on.
  property var enabled: []
  // What the outputs that are on are called in the menu, e.g. "Laptop + Monitor", for the Display row.
  property string summary: ""

  function refresh() {
    outputs.refresh();
  }

  // Whether an entry's requirements hold: the outputs it needs connected, and those it needs on.
  function available(requires) {
    return Array.from(requires?.connected ?? []).every(output => root.connected.includes(output))
      && Array.from(requires?.enabled ?? []).every(output => root.enabled.includes(output));
  }

  function names(name, output) {
    return [name, `${output.make} ${output.model} ${output.serial ?? "Unknown"}`];
  }

  // niri reports an output that is off without a logical size.
  CommandReader {
    id: outputs
    shell: Config.commands.outputs
    onRead: text => {
      const all = Object.entries(JSON.parse(text));
      root.connected = [].concat(...all.map(([name, output]) => root.names(name, output)));
      const on = all.filter(([, output]) => output.logical);
      root.enabled = [].concat(...on.map(([name, output]) => root.names(name, output)));
      const labels = Config.display.labels;
      root.summary = on.map(([name, output]) => labels[name] ?? labels[root.names(name, output)[1]] ?? name).sort().join(" + ");
    }
  }
}
