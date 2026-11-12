import QtQuick
import QtQuick.Layouts
import qs

// Keys a level takes beyond its rows, on one line: each key as KeyHint shows it, then what it does.
RowLayout {
  id: root

  // [{ key, what }], a key being a name or a pair of names (["Left", "Right"]).
  property var keys: []

  spacing: 2 * Config.theme.spacing

  Repeater {
    model: root.keys

    RowLayout {
      id: entry
      required property var modelData
      readonly property string label: MenuKeys.label(modelData.key, true)

      spacing: Config.theme.spacing

      KeyHint {
        key: entry.modelData.key
        paired: true
        chars: entry.label.length
      }

      Label {
        tone: "dim"
        text: entry.modelData.what
      }
    }
  }
}
