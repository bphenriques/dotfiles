import QtQuick
import qs

// Something that fades in and out: `value` follows `shown` over theme.animationDuration, for an opacity;
// `loaded` holds until it has faded out, for a LazyLoader, so leaving is seen rather than cut.
Item {
  property bool shown: false
  property real value: shown ? 1 : 0
  readonly property bool loaded: value > 0

  Behavior on value {
    NumberAnimation {
      duration: Config.theme.animationDuration
    }
  }
}
