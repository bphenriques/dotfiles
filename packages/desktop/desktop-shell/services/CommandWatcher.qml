import QtQuick
import Quickshell.Io

// A command from the config that runs for as long as it lasts, read line by line; restarted a moment after it exits, so a command that fails at once cannot loop.
Process {
  id: root

  property string shell

  signal read(string line)

  // The command is set here rather than by a binding: a running binding can turn true before a command binding has it.
  function start() {
    if (!shell) return;
    command = ["sh", "-c", shell];
    running = true;
  }

  Component.onCompleted: start()

  // A new command replaces the running one once it exits.
  onShellChanged: if (running) running = false; else start()

  onExited: if (shell) restart.restart()

  stdout: SplitParser {
    onRead: line => root.read(line)
  }

  property Timer restart: Timer {
    interval: 2000
    onTriggered: root.start()
  }
}
