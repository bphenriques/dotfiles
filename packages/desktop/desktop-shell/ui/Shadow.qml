import QtQuick
import QtQuick.Effects

// A shadow like niri's default, for a surface drawn inside a larger window, where the compositor's own would outline the window.
RectangularShadow {
  offset.y: 5
  spread: 5
  blur: 30
  color: "#77000000"
}
