pragma Singleton

import QtQuick
import Quickshell
import qs

// The outputs, read on refresh(), and the display layout.
Singleton {
  id: root

  // The layout the screens are in.
  readonly property string layout: internal.layout

  function refresh() {
    Compositor.refresh();
    layoutStatus.refresh();
  }

  // Whether an entry's requirements hold: the outputs it needs connected, and those it needs on.
  function available(requires) {
    return Array.from(requires?.connected ?? []).every(output => Compositor.connected.includes(output))
      && Array.from(requires?.enabled ?? []).every(output => Compositor.enabled.includes(output));
  }

  function allows(layout) {
    return !layout || internal.layouts.includes(layout);
  }

  // The layouts the connected screens allow.
  QtObject {
    id: internal
    property string layout: ""
    property var layouts: []
  }

  CommandReader {
    id: layoutStatus
    shell: Config.display.status
    onRead: text => {
      const [current, ...allowed] = text.split("\n");
      internal.layout = current;
      internal.layouts = allowed.filter(layout => layout);
    }
  }
}
