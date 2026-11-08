import QtQuick

// A MouseArea that turns the wheel into steps, one per notch, up or down: touchpads send many small deltas.
MouseArea {
  id: root

  property int delta: 0

  signal stepped(bool up)

  onWheel: wheel => {
    delta += wheel.angleDelta.y;
    while (Math.abs(delta) >= 120) {
      root.stepped(delta > 0);
      delta -= Math.sign(delta) * 120;
    }
  }
}
