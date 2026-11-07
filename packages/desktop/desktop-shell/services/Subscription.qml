import QtQuick
import Quickshell

// Keeps a service's on-demand sources open (its `subscribers` above zero) while this object lives and is active.
Scope {
  id: root

  required property QtObject service
  property bool active: true

  function sync() {
    if (!service || active === state.counted) return;
    state.counted = active;
    service.subscribers += active ? 1 : -1;
  }

  onActiveChanged: sync()
  Component.onCompleted: sync()
  Component.onDestruction: if (state.counted) service.subscribers--

  QtObject {
    id: state
    property bool counted: false
  }
}
