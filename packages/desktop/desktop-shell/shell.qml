import QtQuick
import Quickshell
import qs
import qs.services
import qs.widgets
import qs.widgets.menu

ShellRoot {
  // Singletons are created on first use: Config must load before any surface needs it, and Tray must host before apps register.
  Component.onCompleted: {
    Config.theme;
    Tray.items;
  }

  Menu {}

  Osd {}

  WorkspaceMap {}

  RecordingIndicator {}
}
