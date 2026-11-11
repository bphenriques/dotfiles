import QtQuick
import qs

// A panel: the theme's background, outlined in primary.
Rectangle {
  color: Tone.surface(Config.theme.surfaceOpacity)
  border.color: Config.theme.primary
  border.width: Config.theme.borderWidth
  radius: Config.theme.radius
}
