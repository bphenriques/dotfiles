pragma Singleton

import QtQuick
import Quickshell
import Quickshell.Io
import qs

// Events from Config.calendar.file (README.md#calendar), read again whenever the file is replaced.
Singleton {
  id: root

  // Whether the file has been read; until then the month shows alone.
  readonly property bool enabled: store.loaded
  // When the file was written, or null without one.
  readonly property var updated: store.updated
  // Whether a sync is running, and why the last one failed ("" when it did not): the file's own word.
  readonly property bool syncing: store.syncing
  readonly property string error: store.error
  // Every day with events, in order, each event once per day it falls on: { id, key, day, event }. `key` names the day
  // (2026-11-12) and `id` the row, the same across reads of the file while the event is.
  readonly property var timeline: store.timeline
  // The next event not over yet, timed ones first: an all-day one (a daily habit) would otherwise hold it every day.
  readonly property var next: store.events.find(event => !event.allDay && event.end > Clock.date)
    ?? store.events.find(event => event.end > Clock.date) ?? null

  function key(day) {
    return Qt.formatDate(day, "yyyy-MM-dd");
  }

  // The events on a day, all-day first, then by start.
  function eventsOn(day) {
    return store.days[key(day)] ?? [];
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
    property bool syncing: false
    property string error: ""
    // By start.
    property var events: []
    // By day key.
    property var days: ({})
    property var timeline: []
    // The events reminded of, by id, so each is once.
    property var reminded: ({})

    function read(json) {
      events = Array.from(json.events ?? []).map(event => {
        const start = root.parse(event.start);
        const end = root.parse(event.end);
        // An all-day event's end is its last day; it runs to the start of the next.
        if (event.allDay) end.setDate(end.getDate() + 1);
        const title = root.line(event.title);
        return { id: [event.start, event.color, title].join(" "), title, start, end, allDay: !!event.allDay, color: event.color ?? "", location: root.line(event.location) };
      }).sort((a, b) => a.start - b.start);
      // Each event under every day it overlaps; one without length (start equals end) under the day it starts on.
      const byDay = {};
      for (const event of events) {
        const last = event.end > event.start ? new Date(event.end - 1) : event.start;
        for (let day = new Date(event.start.getFullYear(), event.start.getMonth(), event.start.getDate()); day <= last;
             day = new Date(day.getFullYear(), day.getMonth(), day.getDate() + 1)) {
          const key = root.key(day);
          if (!byDay[key]) byDay[key] = [];
          byDay[key].push(event);
        }
      }
      const rows = [];
      for (const key of Object.keys(byDay).sort()) {
        byDay[key].sort((a, b) => (b.allDay - a.allDay) || (a.start - b.start));
        const day = root.parse(key);
        for (const event of byDay[key]) rows.push({ id: key + " " + event.id, key, day, event });
      }
      days = byDay;
      timeline = rows;
      updated = json.updated ? root.parse(json.updated) : null;
      syncing = !!json.syncing;
      error = json.error ?? "";
      loaded = true;
    }

    function clear() {
      events = [];
      days = {};
      timeline = [];
      updated = null;
      syncing = false;
      error = "";
      loaded = false;
    }
  }

  // Each timed event once, Config.calendar.remindBefore minutes ahead, on the clock's own minute.
  function remind() {
    if (!Config.calendar.remind) return;
    const now = Clock.date;
    const until = new Date(now.getTime() + Config.calendar.remindBefore * 60000);
    for (const event of store.events) {
      if (event.allDay || event.start <= now || event.start > until || store.reminded[event.id]) continue;
      store.reminded[event.id] = true;
      const when = Clock.format(event.start, "%H:%M") + (event.location ? " · " + event.location : "");
      Quickshell.execDetached(["sh", "-c", Config.calendar.remind + ' "$1" "$2"', "sh", event.title, when]);
    }
  }

  Connections {
    target: Clock

    function onDateChanged() {
      root.remind();
    }
  }

  FileView {
    path: Config.calendar.file
    watchChanges: true
    printErrors: false
    onFileChanged: reload()
    onLoaded: {
      try {
        store.read(JSON.parse(text()));
      } catch (error) {
        // A file that cannot be read keeps what was shown.
        console.warn("calendar:", Config.calendar.file + ":", error);
      }
    }
    onLoadFailed: store.clear()
  }
}
