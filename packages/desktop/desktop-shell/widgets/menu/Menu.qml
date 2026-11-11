pragma ComponentBehavior: Bound

import QtQuick
import QtQuick.Layouts
import Quickshell
import Quickshell.Io
import Quickshell.Wayland
import qs
import qs.services
import qs.ui

// The keyboard menu with live status: entries come from my.programs.desktop-shell.menu.entries.
Scope {
  id: root

  readonly property bool open: menu.open

  function ordinal(day) {
    if (day >= 11 && day <= 13) return "th";
    return ["th", "st", "nd", "rd"][day % 10] ?? "th";
  }

  // A row of the menu: its key, then its status or label.
  component MenuRow: ListItem {
    id: row
    required property var modelData
    property int chars: 1

    enabled: modelData.enabled !== false
    onActivated: menu.activate(modelData)
    onSecondaryActivated: menu.openMenu(modelData)
    onScrolled: up => menu.act(modelData, up ? "Up" : "Down")

    KeyHint {
      key: row.modelData.key
      paired: row.modelData.paired ?? false
      chars: row.chars
    }

    MenuStatus {
      Layout.fillWidth: true
      image: row.modelData.image ?? ""
      status: row.modelData.status ?? ""
      fallback: row.modelData.desc
      fallbackDetail: row.modelData.detail ?? ""
      icon: row.modelData.icon ?? ""
      active: row.modelData.active ?? false
      pins: row.modelData.entry?.pins ?? []
    }
  }

  MenuModel {
    id: menu
  }

  Binding {
    target: Audio
    property: "live"
    value: menu.open
  }

  Binding {
    target: Audio
    property: "metering"
    value: menu.level?.status === "source"
  }

  IpcHandler {
    target: "menu"

    function toggle(): void {
      menu.toggle();
    }

    function toggleEntry(key: string): void {
      menu.toggleEntry(key);
    }
  }

  LazyLoader {
    id: dismiss
    active: menu.open

    Variants {
      model: Quickshell.screens

      DismissArea {
        required property var modelData
        screen: modelData
        onDismissed: menu.hide()
      }
    }
  }

  // Created after the dismiss areas, so it stacks above them on the same layer.
  LazyLoader {
    active: menu.open && dismiss.item !== null

    OverlayWindow {
      // The whole screen, so levels of different sizes never resize the window: a resize shows the old size for a frame.
      anchors.top: true
      anchors.bottom: true
      anchors.left: true
      anchors.right: true
      WlrLayershell.namespace: "menu"
      WlrLayershell.keyboardFocus: WlrKeyboardFocus.Exclusive
      // Clicks beside the card reach the dismiss area below.
      mask: Region {
        item: card
      }
      BackgroundEffect.blurRegion: Region {
        item: card
        radius: Config.theme.radius
      }

      Shadow {
        anchors.fill: card
        radius: card.radius
      }

      SystemClock {
        id: clock
        precision: SystemClock.Minutes
      }

      Surface {
        id: card
        anchors.horizontalCenter: parent.horizontalCenter
        anchors.bottom: parent.bottom
        anchors.bottomMargin: Config.theme.edgeMargin
        width: menu.width
        height: content.implicitHeight + 2 * Config.theme.padding

        focus: true
        Keys.onPressed: event => event.accepted = menu.handleKey(event)

        ColumnLayout {
          id: content
          anchors.verticalCenter: parent.verticalCenter
          anchors.left: parent.left
          anchors.right: parent.right
          anchors.margins: Config.theme.padding
          spacing: Config.theme.padding

          // The clock on the left, steady state and alerts on the right; inside a level its title and status.
          RowLayout {
            Layout.rightMargin: Config.theme.itemPaddingX
            spacing: Config.theme.spacing

            // Inside a level the title goes back up, like Backspace.
            ListItem {
              enabled: menu.level !== null
              dimsWhenDisabled: false
              onActivated: menu.back()

              Label {
                font.bold: true
                text: menu.level?.title ?? Qt.formatDateTime(clock.date, "dddd, d'" + root.ordinal(clock.date.getDate()) + "' MMMM hh:mm")
              }
            }

            Label {
              visible: menu.level?.entry?.list === "tray"
              tone: "dim"
              text: "Ctrl+digit: app menu"
            }

            Item {
              Layout.fillWidth: true
            }

            // Always shown, so their state is known at a glance: lit when on, dim when off.
            Icon {
              visible: menu.level === null
              tone: DoNotDisturb.enabled ? "alert" : "dim"
              glyph: DoNotDisturb.enabled ? Glyphs.doNotDisturb : Glyphs.notifications
            }

            Icon {
              visible: menu.level === null
              tone: IdleInhibit.enabled ? "primary" : "dim"
              glyph: Glyphs.keepAwake
            }

            MenuStatus {
              Layout.fillWidth: false
              visible: menu.level === null
              quiet: true
              status: "layout"
            }

            // A level that lists its items (tray apps, devices, networks) shows only its state, not the items again.
            MenuStatus {
              Layout.fillWidth: false
              quiet: true
              compact: !!menu.level?.entry?.list
              status: menu.level === null ? "battery" : menu.level.entry?.list === "tray" ? "" : menu.level.status ?? ""
            }
          }

          // Declared, not inferred: the level's own columns, else menu.columns; entries flow across, so neighbours sit side by side.
          // Hidden when empty: an empty GridLayout keeps the height of its last rows.
          GridLayout {
            id: actionsGrid
            Layout.fillWidth: true
            visible: menu.actions.length > 0
            flow: GridLayout.LeftToRight
            columns: menu.level?.entry?.columns ?? Config.menu.columns
            columnSpacing: Config.theme.padding
            rowSpacing: Config.theme.listSpacing

            Repeater {
              model: menu.actions

              MenuRow {
                required property int index
                Layout.fillWidth: true
                Layout.preferredWidth: 1
                chars: menu.columnChars(index, actionsGrid.columns)
              }
            }
          }

          Divider {
            Layout.fillWidth: true
            visible: menu.actions.length > 0 && menu.rows.length > 0
          }

          // What a level lists, apart from its actions and across the full width.
          ColumnLayout {
            Layout.fillWidth: true
            visible: menu.rows.length > 0
            spacing: Config.theme.listSpacing

            Repeater {
              model: menu.rows

              MenuRow {
                Layout.fillWidth: true
                chars: menu.keyChars(menu.rows)
              }
            }
          }
        }
      }
    }
  }
}
