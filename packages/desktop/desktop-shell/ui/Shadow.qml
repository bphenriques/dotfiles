import QtQuick
import QtQuick.Effects

// niri's default shadow, for a surface drawn inside a larger window, where niri's own would outline the window.
RectangularShadow {
  offset.y: 5
  spread: 5
  blur: 30
  color: "#77000000"
}
