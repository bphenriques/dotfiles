import QtQuick
import Quickshell.Io

// A command from the config, run on refresh() and read whole. A request while it runs runs it once more afterwards, so the last state wins.
Process {
  id: root

  property string shell
  // Also run once the command is known, for state the shell needs before the menu opens.
  property bool atStart: false
  property bool pending: false

  signal read(string text)

  // The command is set here rather than by a binding: a running binding can turn true before a command binding has it.
  function refresh() {
    if (!shell) return;
    command = ["sh", "-c", shell];
    if (running) {
      pending = true;
    } else {
      running = true;
    }
  }

  Component.onCompleted: if (atStart) refresh()
  onShellChanged: if (atStart) refresh()

  onExited: if (pending) {
    pending = false;
    running = true;
  }

  stdout: StdioCollector {
    onStreamFinished: root.read(text)
  }
}
