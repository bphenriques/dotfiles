import QtQuick
import QtQuick.Layouts
import Quickshell
import Quickshell.Io
import Quickshell.Wayland
import qs
import qs.services
import qs.ui

// wlr-which-key with live status: entries come from my.programs.desktop-shell.menu.entries.
Scope {
  id: root

  MenuModel {
    id: menu
  }

  IpcHandler {
    target: "menu"

    function toggle(): void {
      menu.toggle();
    }
  }

  LazyLoader {
    active: menu.open

    DismissArea {
      onDismissed: menu.hide()
    }
  }

  LazyLoader {
    active: menu.open

    OverlayWindow {
      anchors.bottom: true
      margins.bottom: Config.theme.edgeMargin
      WlrLayershell.namespace: "menu"
      WlrLayershell.keyboardFocus: WlrKeyboardFocus.Exclusive

      implicitWidth: card.width
      implicitHeight: card.height

      Subscription {
        service: Audio
      }

      Subscription {
        service: Niri
      }

      Subscription {
        service: Display
      }

      SystemClock {
        id: clock
        precision: SystemClock.Minutes
      }

      Surface {
        id: card
        width: Config.menu.width
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

          // The clock and alerts on the left, steady state on the right; inside a level its title and status.
          RowLayout {
            Layout.rightMargin: Config.theme.itemPaddingX
            spacing: Config.theme.spacing

            // Inside a level the title goes back up, like Backspace.
            ListItem {
              enabled: menu.level !== null
              onActivated: menu.back()

              Label {
                font.bold: true
                text: menu.level?.title ?? Qt.formatDateTime(clock.date, "ddd dd MMM  hh:mm")
              }
            }

            Label {
              visible: menu.level === null && DoNotDisturb.enabled
              tone: "alert"
              text: Glyphs.doNotDisturb
            }

            Item {
              Layout.fillWidth: true
            }

            MenuStatus {
              Layout.fillWidth: false
              visible: menu.level === null
              status: "layout"
            }

            MenuStatus {
              Layout.fillWidth: false
              status: menu.level === null ? "battery" : menu.level.status ?? ""
            }
          }

          // Laid out like wlr-which-key: columns of a fixed number of rows.
          GridLayout {
            Layout.fillWidth: true
            flow: GridLayout.TopToBottom
            rows: Math.max(1, Math.min(Config.menu.rowsPerColumn, menu.items.length))
            columnSpacing: Config.theme.padding
            rowSpacing: Config.theme.listSpacing

            Repeater {
              model: menu.items

              ListItem {
                id: item
                required property var modelData

                Layout.fillWidth: true
                Layout.preferredWidth: 1
                onActivated: menu.activate(modelData)
                onSecondaryActivated: menu.openMenu(modelData)
                onScrolled: up => menu.act(modelData, up ? "Up" : "Down")

                KeyHint {
                  key: item.modelData.key
                }

                MenuStatus {
                  Layout.fillWidth: true
                  image: item.modelData.image ?? ""
                  status: item.modelData.status ?? ""
                  fallback: item.modelData.desc
                  fallbackDetail: item.modelData.detail ?? ""
                  icon: item.modelData.icon ?? ""
                }
              }
            }
          }
        }
      }
    }
  }
}
