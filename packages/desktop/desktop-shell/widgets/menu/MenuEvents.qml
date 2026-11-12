pragma ComponentBehavior: Bound

import QtQuick
import QtQuick.Layouts
import qs
import qs.services
import qs.ui

// A list of events under a heading per day, as services/Calendar.qml gives it (one day's, or the agenda): a bar
// in the event's colour, start over end, then title over location, one line when neither has a second. It fills the
// height it is given, keeps the event in focus in view, and gives the first one shown its day's heading; a count of
// where it is, out of all, shows once they do not all fit.
ColumnLayout {
  id: root

  // { day } headings and { day, event } events, in order.
  required property var items
  // What stands for the events when there are none.
  required property string emptyText
  // The event in focus, by its place among the events; -1 for none.
  property int focusedEvent: -1
  // The first item shown, moved only to keep the event in focus in view.
  property int start: 0

  // Where each event sits in items.
  readonly property var eventAt: items.map((item, i) => item.event ? i : -1).filter(i => i >= 0)
  readonly property real line: probe.implicitHeight
  readonly property real budget: list.height
  readonly property int count: fitting(start)
  // The items shown as { item, at }, at being the place in items; the heading carried over a scroll has none.
  readonly property var shown: (items[start]?.event ? [{ item: { day: items[start].day }, at: -1 }] : [])
    .concat(items.slice(start, start + count).map((item, i) => ({ item, at: start + i })))
  // The place of the event in focus, else of the last one shown, counting from 1.
  readonly property int position: focusedEvent >= 0 ? focusedEvent + 1 : items.slice(0, start + count).filter(item => item.event).length

  spacing: Config.theme.spacing / 2

  onItemsChanged: {
    start = 0;
    reveal();
  }
  onFocusedEventChanged: reveal()

  // The left column's two lines on `day`: "All day" for an event that fills it, with its last day when it runs past,
  // else start and end, `…` for what falls on another day.
  function times(event, day) {
    const dayStart = new Date(day.getFullYear(), day.getMonth(), day.getDate());
    const dayEnd = new Date(day.getFullYear(), day.getMonth(), day.getDate() + 1);
    // Ending at midnight, an event ends on the day before.
    const last = new Date(event.end - 1);
    if (event.allDay || (event.start <= dayStart && event.end >= dayEnd))
      return ["All day", event.end > dayEnd ? "to " + Clock.format(last, "%a %-d") : ""];
    return [
      event.start < dayStart ? "…" : Clock.format(event.start, "%H:%M"),
      event.end > dayEnd ? "…" : event.end > event.start ? Clock.format(event.end, "%H:%M") : ""
    ];
  }

  // The height of `rows` two-line events, with the count below them.
  function rowsHeight(rows) {
    return rows * (2 * line + 2 * Config.theme.itemPaddingY) + rows * spacing + line;
  }

  function compact(item) {
    return !times(item.event, item.day)[1] && !item.event.location;
  }

  function heightOf(item) {
    return item.event ? (compact(item) ? 1 : 2) * line + 2 * Config.theme.itemPaddingY : line + Config.theme.itemPaddingY;
  }

  // How many items from `from` fit, after the heading carried over when `from` is an event.
  function fitting(from) {
    let used = items[from]?.event ? heightOf({ day: items[from].day }) : 0;
    let fit = 0;
    for (let i = from; i < items.length; i++) {
      const height = heightOf(items[i]) + (used > 0 ? spacing : 0);
      // A heading only goes in with its day's first event, if it has one.
      const first = items[i].event || !items[i + 1] ? 0 : spacing + heightOf(items[i + 1]);
      if (used + height + first > budget) break;
      used += height;
      fit++;
    }
    return fit;
  }

  // Keeps the event in focus in view; with none, the list starts over.
  function reveal() {
    const at = eventAt[focusedEvent];
    if (at === undefined) {
      start = 0;
      return;
    }
    // Going up, an event that opens its day takes its heading along.
    if (at < start) start = at > 0 && !items[at - 1].event ? at - 1 : at;
    while (at >= start + fitting(start)) start++;
  }

  function heading(day) {
    const tomorrow = new Date(Clock.date.getFullYear(), Clock.date.getMonth(), Clock.date.getDate() + 1);
    return day.toDateString() === Clock.date.toDateString() ? "Today" : day.toDateString() === tomorrow.toDateString() ? "Tomorrow"
      : Clock.format(day, "%A %-d %B");
  }

  TextMetrics {
    id: cell
    font.family: Config.theme.monospaceFont
    font.pointSize: Config.theme.fontSize
    text: "0"
  }

  Label {
    id: probe
    visible: false
    text: "0"
  }

  ColumnLayout {
    id: list
    Layout.fillWidth: true
    Layout.fillHeight: true
    Layout.preferredHeight: 0
    clip: true
    spacing: root.spacing

    Repeater {
      model: root.shown

      Item {
        id: slot
        required property var modelData
        readonly property var item: modelData.item
        readonly property var event: item.event ?? null
        readonly property var lines: event ? root.times(event, item.day) : ["", ""]
        readonly property bool compact: !!event && root.compact(item)
        readonly property bool focused: !!event && root.eventAt.indexOf(modelData.at) === root.focusedEvent && root.focusedEvent >= 0

        Layout.fillWidth: true
        implicitHeight: root.heightOf(item)

        Label {
          visible: !slot.event
          anchors.bottom: parent.bottom
          anchors.left: parent.left
          anchors.leftMargin: Config.theme.itemPaddingX
          tone: "dim"
          font.bold: true
          text: root.heading(slot.item.day)
        }

        Rectangle {
          anchors.fill: parent
          visible: slot.focused
          radius: Config.theme.itemRadius
          color: Qt.alpha(Config.theme.primary, Config.theme.highlightOpacity)
        }

        RowLayout {
          anchors.verticalCenter: parent.verticalCenter
          anchors.left: parent.left
          anchors.right: parent.right
          anchors.leftMargin: Config.theme.itemPaddingX
          anchors.rightMargin: Config.theme.itemPaddingX
          spacing: Config.theme.spacing
          visible: !!slot.event

          Swatch {
            Layout.fillHeight: true
            size: Config.theme.fontSize / 4
            fill: slot.event?.color ?? "transparent"
          }

          // As wide as "to Wed 30", so titles start together.
          ColumnLayout {
            Layout.minimumWidth: 9 * cell.advanceWidth
            Layout.maximumWidth: Layout.minimumWidth
            Layout.alignment: Qt.AlignTop
            spacing: 0

            Label {
              text: slot.lines[0]
            }

            Label {
              visible: !slot.compact
              tone: "dim"
              text: slot.lines[1]
            }
          }

          ColumnLayout {
            Layout.fillWidth: true
            Layout.alignment: Qt.AlignTop
            spacing: 0

            Label {
              Layout.fillWidth: true
              elide: Text.ElideRight
              text: slot.event?.title ?? ""
            }

            Label {
              visible: !slot.compact
              Layout.fillWidth: true
              elide: Text.ElideRight
              tone: "dim"
              text: slot.event?.location ?? ""
            }
          }
        }
      }
    }

    Label {
      visible: root.eventAt.length === 0
      Layout.leftMargin: Config.theme.itemPaddingX
      Layout.topMargin: Config.theme.itemPaddingY
      tone: "dim"
      text: root.emptyText
    }

    Item {
      Layout.fillHeight: true
    }
  }

  Label {
    Layout.leftMargin: Config.theme.itemPaddingX
    tone: "dim"
    // Empty rather than hidden, so the list's height holds.
    text: root.focusedEvent >= 0 || root.position < root.eventAt.length ? root.position + "/" + root.eventAt.length : ""
  }
}
