import QtQuick
import qs

// A panel: the theme's background, outlined in primary.
Rectangle {
  color: Tone.surface()
  border.color: Tone.color("primary")
  border.width: Config.theme.borderWidth
  radius: Config.theme.radius
}
