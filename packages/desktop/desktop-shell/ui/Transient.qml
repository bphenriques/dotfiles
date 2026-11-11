import QtQuick
import qs

// Something shown for a moment: show() shows it for Config.osd.timeout, held on while `held`.
// show() does nothing until `armed`, so state replayed at startup is not shown as a change.
Timer {
  id: root

  property bool shown: false
  property bool held: false
  property bool armed: true

  function show() {
    if (!armed) return;
    shown = true;
    restart();
  }

  function hide() {
    stop();
    shown = false;
  }

  interval: Config.osd.timeout
  onTriggered: held ? restart() : hide()
}
