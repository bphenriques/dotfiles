import QtQuick
import qs

// A dot in a colour that is data, not the theme's (a calendar's), or a bar when stretched; a dim tone dims it like text.
Rectangle {
  property color fill
  property string tone: "normal"
  property real size: Config.theme.fontSize

  implicitWidth: size
  implicitHeight: size
  radius: Math.min(width, height) / 2
  color: tone === "dim" ? Qt.alpha(fill, Config.theme.dimOpacity) : fill
}
