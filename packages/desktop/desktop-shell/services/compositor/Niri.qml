pragma Singleton

import QtQuick
import Quickshell
import Quickshell.Io

// The niri backend of Compositor: keyboard layouts followed through its event stream, which replays everything on connect, and outputs on request.
// `received` passes on every event, for integrations built on niri.
Singleton {
  id: root

  readonly property var layouts: internal.layouts
  readonly property int layoutIndex: internal.layoutIndex

  // Every event as niri sends it, for integrations that follow more of niri than the shell does.
  signal received(var event)

  QtObject {
    id: internal

    property var layouts: []
    property int layoutIndex: 0

    function handle(event) {
      if (event.KeyboardLayoutsChanged) {
        layouts = event.KeyboardLayoutsChanged.keyboard_layouts.names;
        layoutIndex = event.KeyboardLayoutsChanged.keyboard_layouts.current_idx;
      } else if (event.KeyboardLayoutSwitched) {
        layoutIndex = event.KeyboardLayoutSwitched.idx;
      }
      root.received(event);
    }
  }

  // One request on its own connection, as `niri msg` does: the event stream's connection carries only events.
  function request(message, callback) {
    requestComponent.createObject(root, { message: JSON.stringify(message), callback: callback ?? null });
  }

  function switchLayout() {
    request({ Action: { SwitchLayout: { layout: "Next" } } });
  }

  // Calls back with the outputs connected and those on, each by connector name and by make, model and serial; one that is off has no logical size.
  function outputs(onRead) {
    request("Outputs", reply => {
      const all = Object.entries(reply.Outputs);
      const names = ([name, output]) => [name, `${output.make} ${output.model} ${output.serial ?? "Unknown"}`];
      onRead([].concat(...all.map(names)), [].concat(...all.filter(([, output]) => output.logical).map(names)));
    });
  }

  Component {
    id: requestComponent

    Socket {
      id: socket
      property string message
      property var callback
      // One that never connects or answers is dropped, rather than kept for good.
      property Timer expiry: Timer {
        interval: 5000
        running: true
        onTriggered: socket.destroy()
      }

      path: Quickshell.env("NIRI_SOCKET")
      connected: true
      onConnectedChanged: {
        if (connected) {
          write(message + "\n");
          flush();
        } else {
          destroy();
        }
      }

      parser: SplitParser {
        onRead: line => {
          const reply = JSON.parse(line);
          if (reply.Ok && socket.callback) socket.callback(reply.Ok);
          socket.connected = false;
        }
      }
    }
  }

  Socket {
    path: Quickshell.env("NIRI_SOCKET")
    connected: true
    onConnectedChanged: if (connected) write('"EventStream"\n')

    parser: SplitParser {
      onRead: line => internal.handle(JSON.parse(line))
    }
  }
}
