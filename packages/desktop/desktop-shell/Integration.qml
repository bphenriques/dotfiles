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
}
