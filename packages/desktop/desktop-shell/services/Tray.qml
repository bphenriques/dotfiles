pragma Singleton

import QtQuick
import Quickshell
import Quickshell.Services.SystemTray

// Apps' tray icons (StatusNotifierItem). Apps register only with a running host, so this is created at startup.
Singleton {
  readonly property var items: Array.from(SystemTray.items.values).sort((a, b) => (a.title || a.id).localeCompare(b.title || b.id))
}
