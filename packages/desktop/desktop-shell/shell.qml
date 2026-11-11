import QtQuick
import Quickshell
import qs
import qs.services
import qs.widgets
import qs.widgets.menu

ShellRoot {
  // Singletons are created on first use: Config must load before any surface needs it, Tray must host before apps register, and integrations load.
  Component.onCompleted: {
    Config.theme;
    Tray.items;
    Integrations.get;
  }

  Menu {}

  Osd {}
}
