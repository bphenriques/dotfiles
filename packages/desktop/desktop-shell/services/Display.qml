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
  // The layout the screens are in, and the layouts the connected screens allow.
  property string layout: ""
  property var layouts: []

  function refresh() {
    outputs.refresh();
    layoutStatus.refresh();
  }

  // Whether an entry's requirements hold: the outputs it needs connected, and those it needs on.
  function available(requires) {
    return Array.from(requires?.connected ?? []).every(output => root.connected.includes(output))
      && Array.from(requires?.enabled ?? []).every(output => root.enabled.includes(output));
  }

  function allows(layout) {
    return !layout || root.layouts.includes(layout);
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
      root.enabled = [].concat(...all.filter(([, output]) => output.logical).map(([name, output]) => root.names(name, output)));
    }
  }

  CommandReader {
    id: layoutStatus
    shell: Config.commands.displayLayout
    onRead: text => {
      const [current, ...allowed] = text.split("\n");
      root.layout = current;
      root.layouts = allowed.filter(layout => layout);
    }
  }
}
