import QtQuick
import qs

Rectangle {
  color: Qt.alpha(Config.theme.background, Config.theme.surfaceOpacity)
  border.color: Config.theme.primary
  border.width: Config.theme.borderWidth
  radius: Config.theme.radius
}
