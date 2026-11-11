pragma Singleton

import QtQuick
import Quickshell
import qs

// Loads the integrations Config.integrations names, each from its source: a path, relative to the shell, or by default the one it ships under that name (workspace-map: integrations/WorkspaceMap.qml).
// One that fails to load is logged and left out; the rest of the shell runs on.
Singleton {
  id: root

  function get(name) {
    return internal.loaded[name] ?? null;
  }

  QtObject {
    id: internal

    property var loaded: ({})

    function load(name, spec) {
      const source = spec.source ?? "integrations/" + name.split("-").map(word => word.charAt(0).toUpperCase() + word.slice(1)).join("") + ".qml";
      const component = Qt.createComponent(source.startsWith("/") ? "file://" + source : Quickshell.shellPath(source));
      if (component.status !== Component.Ready) {
        console.warn("integration", name, "not loaded:", component.errorString());
        return null;
      }
      const integration = component.createObject(root, { name: name, settings: spec.settings ?? {} });
      if (!integration) console.warn("integration", name, "not created");
      return integration;
    }

    function reload() {
      for (const integration of Object.values(loaded)) integration.destroy();
      const next = {};
      for (const [name, spec] of Object.entries(Config.integrations)) {
        const integration = load(name, spec);
        if (integration) next[name] = integration;
      }
      loaded = next;
    }
  }

  // The config may have loaded before this singleton was created.
  Component.onCompleted: internal.reload()

  Connections {
    target: Config

    function onIntegrationsChanged() {
      internal.reload();
    }
  }
}
