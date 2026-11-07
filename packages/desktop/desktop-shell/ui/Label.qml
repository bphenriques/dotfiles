import QtQuick
import qs

// Text in the theme font. `tone` gives it meaning: normal, dim, primary, warning or alert.
Text {
  property string tone: "normal"

  color: tone === "primary" ? Config.theme.primary
    : tone === "warning" ? Config.theme.warning
    : tone === "alert" ? Config.theme.alert
    : Config.theme.foreground
  opacity: tone === "dim" ? Config.theme.dimOpacity : 1
  font.family: Config.theme.monospaceFont
  font.pointSize: Config.theme.fontSize
}
