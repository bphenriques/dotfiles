import QtQuick
import Quickshell
import qs
import qs.services
import qs.widgets
import qs.widgets.menu

ShellRoot {
  // Singletons are created on first use: Config must load before any surface needs it, Tray must host before apps register, Power must have UPower's answer before the menu first opens, Calendar reminds with the menu closed, and integrations load.
  Component.onCompleted: {
    Config.theme;
    Tray.items;
    Power.present;
    Calendar.enabled;
    Integrations.get;
  }

  Menu {}

  Osd {}
}
