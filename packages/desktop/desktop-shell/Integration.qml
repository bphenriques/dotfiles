import QtQuick
import Quickshell

// The root of every integration (see README.md#integrations).
Scope {
  // Set by the shell: the integration's name in Config.integrations, and its settings there.
  property string name
  property var settings: ({})

  // What a menu entry with `status` set to the name shows, and whether one with `activeWhen` set to it is lit.
  property string glyph
  property string text
  property string detail
  property bool active: false
  // Its compositor key, shown in the header while Super is held; set from its settings by the module that binds it.
  property string shortcut

  // What a click on its header item does.
  function activate() {}

  // A command from its settings as a Process takes it, the arguments passed as $1, $2... so none is parsed by the shell.
  function commandLine(shell, ...args) {
    return ["sh", "-c", shell + args.map((_, i) => ' "$' + (i + 1) + '"').join(""), "sh", ...args];
  }

  // Runs a command from its settings and forgets it; nothing when unset.
  function exec(shell, ...args) {
    if (shell) Quickshell.execDetached(commandLine(shell, ...args));
  }
}
