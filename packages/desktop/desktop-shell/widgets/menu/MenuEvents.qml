pragma ComponentBehavior: Bound

import QtQuick
import QtQuick.Layouts
import qs
import qs.services
import qs.ui

// Every event, under a heading per day (Calendar.timeline), from the selected day on: it starts at that day, `No events`
// under its heading when it has none, and keeps the event in focus in view as the arrows move through it. Each is a bar in
// its colour, start over end, then title over location, one line when neither has a second. The first one shown always
// has its day's heading.
ColumnLayout {
  id: root

  required property var selected
  // The event in focus, by its place among the events; -1 for none.
  property int focusedEvent: -1
  // The first item shown.
  property int start: 0

  // The timeline, with the selected day in it even when it has no events: { day } headings, { day, event } events, and
  // { day, none } for a day without them.
  readonly property var items: {
    const day = new Date(selected.getFullYear(), selected.getMonth(), selected.getDate());
    const at = Calendar.timeline.findIndex(item => item.day >= day);
    if (at >= 0 && Calendar.timeline[at].day.getTime() === day.getTime()) return Calendar.timeline;
    const before = at < 0 ? Calendar.timeline.length : at;
    return Calendar.timeline.slice(0, before).concat([{ day }, { day, none: true }], Calendar.timeline.slice(before));
  }
  // Where each event sits in items.
  readonly property var eventAt: items.map((item, i) => item.event ? i : -1).filter(i => i >= 0)
  // Where the selected day's heading sits.
  readonly property int dayAt: items.findIndex(item => !item.event && !item.none && item.day.toDateString() === selected.toDateString())
  readonly property real line: probe.implicitHeight
  readonly property int count: fitting(start)
  // The items shown as { item, at }, at being the place in items; the heading carried over a scroll has none.
  readonly property var shown: (items[start]?.event ? [{ item: { day: items[start].day }, at: -1 }] : [])
    .concat(items.slice(start, start + count).map((item, i) => ({ item, at: start + i })))

  spacing: Config.theme.spacing / 2

  // Once the list has caught up with the change: the day, the focus and the list change together.
  onItemsChanged: Qt.callLater(follow)
  onSelectedChanged: Qt.callLater(follow)
  onFocusedEventChanged: Qt.callLater(follow)

  // With an event in focus, keeps it in view; else starts at the selected day.
  function follow() {
    if (list.height <= 0) return;
    const at = eventAt[focusedEvent];
    if (at === undefined) {
      start = Math.max(dayAt, 0);
      return;
    }
    // Going up, an event that opens its day takes its heading along.
    if (at < start) start = at > 0 && !items[at - 1].event ? at - 1 : at;
    while (start < at && at >= start + fitting(start)) start++;
  }

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

  function compact(item) {
    return !times(item.event, item.day)[1] && !item.event.location;
  }

  function heightOf(item) {
    if (item.event) return (compact(item) ? 1 : 2) * line + 2 * Config.theme.itemPaddingY;
    return item.none ? line + 2 * Config.theme.itemPaddingY : line + Config.theme.itemPaddingY;
  }

  // How many items from `from` fit, after the heading carried over when `from` is an event.
  function fitting(from) {
    let used = items[from]?.event ? heightOf({ day: items[from].day }) : 0;
    let fit = 0;
    for (let i = from; i < items.length; i++) {
      const height = heightOf(items[i]) + (used > 0 ? spacing : 0);
      // A heading only goes in with what follows it.
      const next = items[i].event || items[i].none || !items[i + 1] ? 0 : spacing + heightOf(items[i + 1]);
      if (used + height + next > list.height) break;
      used += height;
      fit++;
    }
    return fit;
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

    onHeightChanged: Qt.callLater(root.follow)

    Repeater {
      model: root.shown

      Item {
        id: slot
        required property var modelData
        readonly property var item: modelData.item
        readonly property var event: item.event ?? null
        readonly property var lines: event ? root.times(event, item.day) : ["", ""]
        readonly property bool compact: !!event && root.compact(item)
        readonly property bool focused: root.focusedEvent >= 0 && root.eventAt[root.focusedEvent] === modelData.at

        Layout.fillWidth: true
        implicitHeight: root.heightOf(item)

        Label {
          visible: !slot.event && !slot.item.none
          anchors.bottom: parent.bottom
          anchors.left: parent.left
          anchors.leftMargin: Config.theme.itemPaddingX
          tone: "dim"
          font.bold: true
          text: root.heading(slot.item.day)
        }

        Label {
          visible: !!slot.item.none
          anchors.verticalCenter: parent.verticalCenter
          anchors.left: parent.left
          anchors.leftMargin: Config.theme.itemPaddingX
          tone: "dim"
          text: "No events"
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

    Item {
      Layout.fillHeight: true
    }
  }
}
