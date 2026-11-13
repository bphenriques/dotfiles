import QtQuick
import qs

// Something under way: a glyph turning once every ten animation durations, standing still when animations are off.
Icon {
  id: root

  glyph: Glyphs.loading

  RotationAnimator on rotation {
    from: 0
    to: 360
    duration: Config.theme.animationDuration * 10
    loops: Animation.Infinite
    running: root.visible && Config.theme.animationDuration > 0
  }
}
