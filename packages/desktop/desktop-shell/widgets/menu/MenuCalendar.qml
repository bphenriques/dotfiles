import QtQuick
import QtQuick.Layouts
import qs
import qs.ui

// A month in the system locale's month and weekday names, weeks starting on Config.calendar.firstDayOfWeek.
Item {
  id: root

  required property date today
  // Months away from today's.
  required property int offset

  signal stepped(int months)

  readonly property date shown: new Date(today.getFullYear(), today.getMonth() + offset, 1)
  readonly property int firstDay: Config.calendar.firstDayOfWeek
  // Six weeks from the one holding the 1st, so the grid never changes height.
  readonly property var days: {
    const lead = (shown.getDay() - firstDay + 7) % 7;
    return Array.from({ length: 42 }, (_, i) => new Date(shown.getFullYear(), shown.getMonth(), 1 - lead + i));
  }

  implicitHeight: column.implicitHeight

  ColumnLayout {
    id: column
    anchors.left: parent.left
    anchors.right: parent.right
    spacing: Config.theme.spacing

    RowLayout {
      Layout.leftMargin: Config.theme.itemPaddingX
      spacing: Config.theme.spacing

      KeyHint {
        key: ["Left", "Right"]
        paired: true
        chars: 2
      }

      Label {
        font.bold: true
        text: Qt.locale().standaloneMonthName(root.shown.getMonth()) + " " + root.shown.getFullYear()
      }
    }

    GridLayout {
      Layout.fillWidth: true
      columns: 7
      columnSpacing: 0
      rowSpacing: Config.theme.spacing

      Repeater {
        model: 7

        Label {
          required property int index
          Layout.fillWidth: true
          Layout.preferredWidth: 1
          horizontalAlignment: Text.AlignHCenter
          tone: "dim"
          // Some locales' short names are whole words (pt: segunda), too wide for a column.
          text: Qt.locale().dayName((root.firstDay + index) % 7, Locale.ShortFormat).slice(0, 3)
        }
      }

      Repeater {
        model: root.days

        Label {
          required property date modelData
          readonly property bool isToday: modelData.toDateString() === root.today.toDateString()
          Layout.fillWidth: true
          Layout.preferredWidth: 1
          horizontalAlignment: Text.AlignHCenter
          tone: isToday ? "primary" : modelData.getMonth() === root.shown.getMonth() ? "normal" : "dim"
          font.bold: isToday
          text: modelData.getDate()
        }
      }
    }
  }

  // Up goes back in time, as scrolling up a page would.
  WheelSteps {
    anchors.fill: parent
    onStepped: up => root.stepped(up ? -1 : 1)
  }
}
