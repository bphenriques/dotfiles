pragma Singleton

import QtQuick
import Quickshell
import qs

// The colour of each tone: normal, dim, primary, warning or alert. Dim is the normal colour at reduced alpha,
// so anything coloured by tone dims the same way; widgets never set opacity to dim.
Singleton {
  function color(tone) {
    switch (tone) {
    case "primary": return Config.theme.primary;
    case "warning": return Config.theme.warning;
    case "alert": return Config.theme.alert;
    case "dim": return Qt.alpha(Config.theme.foreground, Config.theme.dimOpacity);
    default: return Config.theme.foreground;
    }
  }

  // The background of a panel, at the opacity given.
  function surface(opacity) {
    return Qt.alpha(Config.theme.background, opacity);
  }
}
