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

  // The background of a panel, at the theme's opacity unless given.
  function surface(opacity = Config.theme.surfaceOpacity) {
    return Qt.alpha(Config.theme.background, opacity);
  }

  // A tone's colour behind something: a highlight, or a meter's track. `strength` multiplies theme.highlightOpacity.
  function wash(tone, strength = 1) {
    return Qt.alpha(color(tone), strength * Config.theme.highlightOpacity);
  }
}
