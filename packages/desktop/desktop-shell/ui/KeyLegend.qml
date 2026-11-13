import QtQuick
import QtQuick.Layouts
import qs

// Keys a level takes beyond its rows, on one line: each key as KeyHint shows it, then what it does; a single key also takes a click.
RowLayout {
  id: root

  // [{ key, what, clickable }], a key being a name or a pair of names (["Left", "Right"]).
  property var keys: []

  signal picked(var key)

  // No spacing: each entry's own padding already spaces them.
  spacing: 0

  Repeater {
    model: root.keys

    // A click does what the key does, where that is one thing (`clickable`): a pair names no one key.
    ListItem {
      id: entry
      required property var modelData
      readonly property string label: MenuKeys.label(modelData.key, true)

      enabled: !!modelData.clickable
      dimsWhenDisabled: false
      onActivated: root.picked(modelData.key)

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
