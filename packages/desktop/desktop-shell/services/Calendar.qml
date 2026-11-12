pragma Singleton

import QtQuick
import Quickshell
import Quickshell.Io
import qs

// Events from Config.calendar.file (README.md#menu), read again whenever the file is replaced.
Singleton {
  id: root

  // Whether the file has been read; until then the month shows alone.
  readonly property bool enabled: store.loaded
  // When the file was written, or null without one.
  readonly property var updated: store.updated
  readonly property var events: store.events
  // Whether a sync is running, and why the last one failed ("" when it did not): the file's own word.
  readonly property bool syncing: store.syncing
  readonly property string error: store.error
  // Every day with events, in order, as one list: a heading { day }, then { day, event } for each of its events. An event
  // over several days is under each of them.
  readonly property var timeline: {
    if (events.length === 0) return [];
    const first = events.reduce((min, event) => event.start < min ? event.start : min, events[0].start);
    const last = events.reduce((max, event) => event.end > max ? event.end : max, events[0].end);
    const items = [];
    for (let day = new Date(first.getFullYear(), first.getMonth(), first.getDate()); day <= last;
         day = new Date(day.getFullYear(), day.getMonth(), day.getDate() + 1)) {
      const on = eventsOn(day);
      if (on.length > 0) items.push({ day }, ...on.map(event => ({ day, event })));
    }
    return items;
  }

  // The events overlapping a day, all-day first, then by start.
  function eventsOn(day) {
    const from = new Date(day.getFullYear(), day.getMonth(), day.getDate());
    const to = new Date(day.getFullYear(), day.getMonth(), day.getDate() + 1);
    // An event without length (start equals end) still belongs to the day it starts on.
    return events.filter(event => event.start < to && (event.end > from || event.start >= from))
      .sort((a, b) => (b.allDay - a.allDay) || (a.start - b.start));
  }

  // The colours of a day's events, each once, in the order eventsOn gives.
  function colorsOn(day) {
    return [...new Set(eventsOn(day).map(event => event.color))];
  }

  // Text on one line, as a row shows it: an address often comes as several.
  function line(text) {
    return (text ?? "").trim().replace(/\s*\n\s*/g, ", ");
  }

  // "2026-11-12" or "2026-11-12T14:00" as local time: Date would read a bare date as UTC.
  function parse(text) {
    const [date, time] = text.split("T");
    const [year, month, day] = date.split("-").map(Number);
    const [hours, minutes] = (time ?? "0:0").split(":").map(Number);
    return new Date(year, month - 1, day, hours, minutes);
  }

  QtObject {
    id: store
    property bool loaded: false
    property var updated: null
    property var events: []
    property bool syncing: false
    property string error: ""
  }

  FileView {
    path: Config.calendar.file
    watchChanges: true
    printErrors: false
    onFileChanged: reload()
    onLoaded: {
      try {
        const json = JSON.parse(text());
        store.events = Array.from(json.events ?? []).map(event => {
          const start = root.parse(event.start);
          const end = root.parse(event.end);
          // An all-day event's end is its last day; it runs to the start of the next.
          if (event.allDay) end.setDate(end.getDate() + 1);
          return { title: root.line(event.title), start, end, allDay: !!event.allDay, color: event.color ?? "", location: root.line(event.location) };
        });
        store.updated = json.updated ? root.parse(json.updated) : null;
        store.syncing = !!json.syncing;
        store.error = json.error ?? "";
        store.loaded = true;
      } catch (error) {
        // A file that cannot be read keeps what was shown.
        console.warn("calendar:", Config.calendar.file + ":", error);
      }
    }
    onLoadFailed: {
      store.events = [];
      store.loaded = false;
      store.updated = null;
      store.syncing = false;
      store.error = "";
    }
  }
}
