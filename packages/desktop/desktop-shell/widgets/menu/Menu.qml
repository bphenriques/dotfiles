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
      fallbackGlyphs: row.modelData.glyphs ?? []
      icon: row.modelData.icon ?? ""
      active: row.modelData.active ?? false
      pins: row.modelData.entry?.pins ?? []
    }
  }

  // Part of the header: its items in a row, aligned left, centre or right. The sides share the width equally, each at least
  // what its items need; the centre takes the width its items want of the rest, shortening its text when crowded.
  // A plain row of equal parts was simpler but cut the clock and could widen the menu; the minimums prevent both.
  component HeaderSlot: Item {
    id: slot
    property var items: []
    property int alignment: Qt.AlignLeft
    readonly property bool centre: alignment === Qt.AlignHCenter
    // The least its items need, their text shortened, never their icons.
    readonly property real contentMinimum: row.Layout.minimumWidth
    // Set on the sides: at least what their items need, or what either side needs when there is a centre to keep centred.
    property real sideMinimum: 0
    property bool centred: false
    default property alias content: row.data

    visible: !centre || slot.items.length > 0
    Layout.fillWidth: true
    Layout.minimumWidth: centre ? contentMinimum : sideMinimum
    // Sides start from what their items want, or both from the same width when keeping a centre centred.
    Layout.preferredWidth: centre || !centred ? row.implicitWidth : sideMinimum
    Layout.maximumWidth: centre ? row.implicitWidth : Number.POSITIVE_INFINITY
    implicitHeight: row.implicitHeight

    RowLayout {
      id: row
      anchors.verticalCenter: parent.verticalCenter
      // Never wider than its share: its items' text gives way instead (see StatusLine).
      width: Math.min(implicitWidth, slot.width)
      x: slot.alignment === Qt.AlignRight ? slot.width - width : slot.alignment === Qt.AlignHCenter ? (slot.width - width) / 2 : 0
      // No spacing: each item's own padding already spaces them.
      spacing: 0

      Repeater {
        model: Array.from(slot.items)

        HeaderItem {}
      }
    }
  }

  // A header item by name: built in, or an integration's. A click does what its compositor key does, shown while Super is held.
  component HeaderItem: ListItem {
    id: item
    required property string modelData
    // Shrinks, never grows: its text gives way before its neighbours.
    Layout.fillWidth: true
    Layout.maximumWidth: implicitWidth
    readonly property bool builtin: ["clock", "notifications", "keepAwake", "keyboardLayout", "battery"].includes(modelData)
    readonly property var integration: builtin ? null : Integrations.get(modelData)
    readonly property string shortcut: integration?.shortcut ?? Config[modelData]?.shortcut ?? ""

    visible: modelData === "keyboardLayout" ? KeyboardLayout.names.length > 0 : modelData === "battery" ? Power.present : builtin || integration !== null
    // The battery only shows state; the clock opens the calendar when there is one.
    enabled: modelData === "clock" ? menu.calendar !== null : modelData !== "battery"
    dimsWhenDisabled: false
    onActivated: {
      switch (modelData) {
      case "clock": menu.activate(menu.calendar); break;
      case "notifications": DoNotDisturb.toggle(); break;
      case "keepAwake": IdleInhibit.toggle(); break;
      case "keyboardLayout": KeyboardLayout.next(); break;
      default: integration?.activate();
      }
    }

    KeyHint {
      visible: menu.shortcutsShown && item.shortcut !== ""
      key: item.shortcut
      chars: MenuKeys.label(item.shortcut).length
    }

    Label {
      visible: item.modelData === "clock"
      Layout.fillWidth: true
      elide: Text.ElideRight
      font.bold: true
      text: Clock.text
    }

    MenuStatus {
      visible: item.modelData !== "clock"
      quiet: true
      status: item.modelData === "clock" ? "" : item.modelData
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

      Surface {
        id: card
        anchors.horizontalCenter: parent.horizontalCenter
        anchors.bottom: parent.bottom
        anchors.bottomMargin: Config.theme.edgeMargin
        width: menu.width
        height: content.implicitHeight + 2 * Config.theme.padding

        function isSuper(event) {
          return event.key === Qt.Key_Super_L || event.key === Qt.Key_Super_R || event.key === Qt.Key_Meta;
        }

        focus: true
        Keys.onPressed: event => {
          if (isSuper(event) && !event.isAutoRepeat) superHeld.start();
          event.accepted = menu.handleKey(event);
        }
        Keys.onReleased: event => {
          if (!isSuper(event)) return;
          superHeld.stop();
          menu.shortcutsShown = false;
        }

        Timer {
          id: superHeld
          interval: 1000
          onTriggered: menu.shortcutsShown = true
        }

        ColumnLayout {
          id: content
          anchors.verticalCenter: parent.verticalCenter
          anchors.left: parent.left
          anchors.right: parent.right
          anchors.margins: Config.theme.padding
          spacing: Config.theme.padding

          // Like waybar's: none grows the menu; in a side too full, its text ends in an ellipsis.
          // At the top level they hold Config.menu.header's items; inside a level, its title on the left and its state on the right.
          RowLayout {
            id: header
            // Both sides as wide when there is a centre to keep centred.
            readonly property bool centred: centreSlot.visible
            readonly property real sideMinimum: Math.max(leftSlot.contentMinimum, rightSlot.contentMinimum)

            Layout.fillWidth: true
            // Never widens the menu: a header whose icons cannot fit overflows instead (rule 4: make the menu wider).
            Layout.minimumWidth: 0
            spacing: Config.theme.spacing

            HeaderSlot {
              id: leftSlot
              centred: header.centred
              sideMinimum: header.centred ? header.sideMinimum : contentMinimum
              items: menu.level ? [] : Config.menu.header.left

              // Inside a level the title goes back up, like Backspace.
              ListItem {
                visible: menu.level !== null
                onActivated: menu.back()

                Label {
                  font.bold: true
                  text: menu.title
                }
              }
            }

            HeaderSlot {
              id: centreSlot
              alignment: Qt.AlignHCenter
              items: menu.level ? [] : Config.menu.header.center
            }

            HeaderSlot {
              id: rightSlot
              centred: header.centred
              sideMinimum: header.centred ? header.sideMinimum : contentMinimum
              alignment: Qt.AlignRight
              items: menu.level ? [] : Config.menu.header.right

              // A level that lists its items (tray apps, devices, networks) shows only its state, not the items again.
              MenuStatus {
                visible: menu.level !== null
                Layout.rightMargin: Config.theme.itemPaddingX
                quiet: true
                compact: !!menu.level?.entry?.list
                status: menu.level?.entry?.list === "tray" ? "" : menu.level?.status ?? ""
              }
            }
          }

          KeyLegend {
            Layout.leftMargin: Config.theme.itemPaddingX
            visible: menu.legend.length > 0
            keys: menu.legend
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
            visible: menu.actions.length > 0 && (menu.rows.length > 0 || menu.emptyText !== "" || menu.level?.entry?.view === "calendar")
          }

          // An empty list says why, in its place.
          Label {
            visible: menu.emptyText !== ""
            Layout.leftMargin: Config.theme.itemPaddingX
            tone: "dim"
            text: menu.emptyText
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

          MenuCalendar {
            Layout.fillWidth: true
            visible: menu.level?.entry?.view === "calendar"
            selected: menu.selected
            onStepped: months => menu.moveSelected(0, months, 0)
            onPicked: day => menu.selected = day
          }
        }
      }
    }
  }
}
