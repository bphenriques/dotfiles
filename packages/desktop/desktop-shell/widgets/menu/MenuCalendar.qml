import QtQuick
import QtQuick.Layouts
import qs
import qs.services
import qs.ui

// The month of the selected day in the clock's locale, weeks starting on its first day (services/Clock.qml).
// Today is lit, the selected day highlighted, days of the months around dim.
Item {
  id: root

  required property var selected

  signal stepped(int months)
  signal picked(var day)

  readonly property int firstDay: Clock.firstDayOfWeek
  // Six weeks from the one holding the 1st, so the grid never changes height.
  readonly property var days: {
    const first = new Date(selected.getFullYear(), selected.getMonth(), 1);
    const lead = (first.getDay() - firstDay + 7) % 7;
    return Array.from({ length: 42 }, (_, i) => new Date(first.getFullYear(), first.getMonth(), 1 - lead + i));
  }

  implicitHeight: grid.implicitHeight

  GridLayout {
    id: grid
    anchors.left: parent.left
    anchors.right: parent.right
    columns: 7
    columnSpacing: 0
    rowSpacing: 0

    Repeater {
      model: 7

      Label {
        required property int index
        Layout.fillWidth: true
        Layout.preferredWidth: 1
        Layout.bottomMargin: Config.theme.itemPaddingY
        horizontalAlignment: Text.AlignHCenter
        tone: "dim"
        // Some locales' short names are whole words (pt: segunda), too wide for a column.
        text: Clock.locale.dayName((root.firstDay + index) % 7, Locale.ShortFormat).slice(0, 3)
      }
    }

    Repeater {
      model: root.days

      Rectangle {
        id: cell
        required property date modelData
        readonly property bool isToday: modelData.toDateString() === Clock.date.toDateString()
        readonly property bool isSelected: modelData.toDateString() === root.selected.toDateString()

        Layout.fillWidth: true
        Layout.preferredWidth: 1
        implicitHeight: day.implicitHeight + 2 * Config.theme.itemPaddingY
        radius: Config.theme.itemRadius
        color: isSelected || mouse.containsMouse ? Qt.alpha(Config.theme.primary, Config.theme.highlightOpacity) : "transparent"

        Label {
          id: day
          anchors.centerIn: parent
          tone: cell.isToday ? "primary" : cell.modelData.getMonth() === root.selected.getMonth() ? "normal" : "dim"
          font.bold: cell.isToday
          text: cell.modelData.getDate()
        }

        MouseArea {
          id: mouse
          anchors.fill: parent
          hoverEnabled: true
          onClicked: root.picked(cell.modelData)
        }
      }
    }
  }

  // Up goes back in time, as scrolling up a page would.
  WheelSteps {
    anchors.fill: parent
    acceptedButtons: Qt.NoButton
    onStepped: up => root.stepped(up ? -1 : 1)
  }
}
