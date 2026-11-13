import QtQuick
import qs

// A wait in one block cell whose `mood` says whose turn it is: `working` spins ("on it"), `attention` knocks ("your
// turn"). Its own tempo, not the theme's; still when animations are off.
Item {
  id: root

  property string mood: "working"
  property string tone: "normal"
  property real size: Config.theme.iconSize

  // Each sequence is frames of [text, milliseconds it holds].
  readonly property var sequences: ({
      working: [["▀", 250], ["▐", 250], ["▄", 250], ["▌", 250]],
      attention: [["█", 150], ["▄", 150], ["█", 150], ["▄", 1200]]
    })

  readonly property var frames: sequences[mood]
  property int frame: 0

  readonly property string text: Config.theme.animationDuration > 0 ? frames[frame][0] : frames[0][0]

  implicitWidth: size
  implicitHeight: size

  onFramesChanged: {
    frame = 0;
    tick.restart();
  }

  Timer {
    id: tick
    running: root.visible && Config.theme.animationDuration > 0
    interval: root.frames[root.frame][1]
    onTriggered: {
      root.frame = (root.frame + 1) % root.frames.length;
      start();
    }
  }

  // Sized so a full block sits inside `size`, clear of a pill's border.
  Label {
    anchors.centerIn: parent
    tone: root.tone
    // A point is 4/3 of a pixel.
    font.pointSize: Math.max(1, root.size * 0.7 * 0.75)
    text: root.text
  }
}
