pragma ComponentBehavior: Bound

import QtQuick
import QtQuick.Layouts
import qs
import qs.services
import qs.ui

// Every event under a heading per day (Calendar.timeline), from the selected day on: it starts at that day, `No events`
// under its heading when it has none, and keeps the event in focus in view. Each row is a bar in the event's colour, start
// over end, then title over location, one line when neither has a second; a click focuses it, as the keys do.
ListView {
  id: root

  required property var selected
  // The row in focus, by its id; "" for none.
  property string focused: ""

  signal picked(var row)

  readonly property string selectedKey: Calendar.key(selected)

  // The timeline, with the selected day in it even when it has no events.
  model: {
    const at = Calendar.timeline.findIndex(row => row.key >= selectedKey);
    if (at >= 0 && Calendar.timeline[at].key === selectedKey) return Calendar.timeline;
    const before = at < 0 ? Calendar.timeline.length : at;
    const day = Calendar.parse(selectedKey);
    return Calendar.timeline.slice(0, before).concat([{ id: "", key: selectedKey, day, event: null }], Calendar.timeline.slice(before));
  }

  clip: true
  spacing: Config.theme.spacing / 2
  boundsBehavior: Flickable.StopAtBounds

  // With an event in focus, keeps it in view; else starts at the selected day.
  // The day's heading stays on top, so the row is placed below it: placed at the very top, the heading would cover it.
  function follow() {
    const at = model.findIndex(row => focused ? row.id === focused : row.key === selectedKey);
    if (at < 0) return;
    positionViewAtIndex(at, focused ? ListView.Contain : ListView.Beginning);
    const item = itemAtIndex(at);
    if (item && item.y - contentY < headingHeight)
      contentY = Math.max(originY, item.y - headingHeight);
  }

  onModelChanged: follow()
  onFocusedChanged: follow()
  onHeightChanged: follow()

  // The left column's two lines on `day`: "All day" for an event that fills it, with its last day when it runs past,
  // else start and end, `…` for what falls on another day.
  function times(event, day) {
    const dayEnd = new Date(day.getFullYear(), day.getMonth(), day.getDate() + 1);
    // Ending at midnight, an event ends on the day before.
    const last = new Date(event.end - 1);
    if (event.allDay || (event.start <= day && event.end >= dayEnd))
      return ["All day", event.end > dayEnd ? "to " + Clock.format(last, "%a %-d") : ""];
    return [
      event.start < day ? "…" : Clock.format(event.start, "%H:%M"),
      event.end > dayEnd ? "…" : event.end > event.start ? Clock.format(event.end, "%H:%M") : ""
    ];
  }

  // Whether something at `y`, `height` tall, ends in view. Only the bottom edge counts: the top row is checked while the
  // list scrolls to a new day, and hiding it for that frame read as a fade.
  function fits(y, height) {
    return y + height <= contentY + root.height + 1;
  }

  function heading(day) {
    const tomorrow = new Date(Clock.date.getFullYear(), Clock.date.getMonth(), Clock.date.getDate() + 1);
    return day.toDateString() === Clock.date.toDateString() ? "Today"
      : day.toDateString() === tomorrow.toDateString() ? "Tomorrow"
      : Clock.format(day, "%A %-d %B");
  }

  // A one-line row's height: what a heading needs below it to be shown.
  readonly property real shortestRow: Metrics.line + 2 * Config.theme.itemPaddingY
  readonly property real headingPadding: Config.theme.spacing / 2
  readonly property real headingHeight: Metrics.line + headingPadding

  section.property: "key"
  // The day of the rows shown stays on top, over them: the first row shown always has its heading.
  section.labelPositioning: ViewSection.InlineLabels | ViewSection.CurrentLabelAtStart
  section.delegate: Rectangle {
    required property string section

    // Only with room for an event under it: a heading alone at the bottom says nothing.
    visible: root.fits(y, height + root.shortestRow)
    width: root.width
    implicitHeight: heading.implicitHeight
    // The card's own colour, so rows scrolling under it are hidden, not shown through.
    color: Tone.surface()

    Label {
      id: heading
      leftPadding: Config.theme.itemPaddingX
      topPadding: root.headingPadding
      tone: "dim"
      font.bold: true
      text: root.heading(Calendar.parse(parent.section))
    }
  }

  delegate: ListItem {
    id: row
    required property var modelData
    readonly property var event: modelData.event
    readonly property var lines: event ? root.times(event, modelData.day) : ["", ""]
    readonly property bool compact: !lines[1] && !event?.location

    width: ListView.view.width
    // Whole or not at all: a sliver of a row reads as a glitch.
    visible: root.fits(y, height)
    enabled: !!event
    dimsWhenDisabled: false
    current: !!event && modelData.id === root.focused
    onActivated: root.picked(modelData)

    Label {
      visible: !row.event
      tone: "dim"
      text: "No events"
    }

    Swatch {
      visible: !!row.event
      Layout.fillHeight: true
      size: Config.theme.fontSize / 4
      fill: row.event?.color ?? "transparent"
    }

    // As wide as "to Wed 30", so titles start together.
    ColumnLayout {
      visible: !!row.event
      Layout.minimumWidth: 9 * Metrics.cell
      Layout.maximumWidth: Layout.minimumWidth
      Layout.alignment: Qt.AlignTop
      spacing: 0

      Label {
        text: row.lines[0]
      }

      Label {
        visible: !row.compact
        tone: "dim"
        text: row.lines[1]
      }
    }

    ColumnLayout {
      visible: !!row.event
      Layout.fillWidth: true
      Layout.alignment: Qt.AlignTop
      spacing: 0

      Label {
        Layout.fillWidth: true
        elide: Text.ElideRight
        text: row.event?.title ?? ""
      }

      Label {
        visible: !row.compact
        Layout.fillWidth: true
        elide: Text.ElideRight
        tone: "dim"
        text: row.event?.location ?? ""
      }
    }
  }
}
