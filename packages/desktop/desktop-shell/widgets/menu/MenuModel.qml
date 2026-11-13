import QtQuick
import Quickshell
import qs
import qs.services
import qs.ui

// The menu's state: whether it is open, the levels opened, and what keys do.
// Every level, static or filled at runtime (tray apps, an app's own menu), is a list of items { key, desc, ... }.
Scope {
  id: root

  property bool open: false
  // Levels opened below the top one, the last is shown: an entry's (kind "entry") or an app's own menu (kind "menu").
  property var stack: []
  // The day selected in a calendar level.
  property var selected: new Date()
  // The calendar's event in focus, by its row id in Calendar.timeline, so a new read of the file keeps it; "" while the keys move the day.
  property string eventFocus: ""
  // Super held a second shows the header's compositor keys, not at once, so a Super shortcut does not flash them.
  property bool shortcutsShown: false

  readonly property var entries: Array.from(Config.menu.entries)
  readonly property var level: stack[stack.length - 1] ?? null
  readonly property var items: itemsOf(level)
  readonly property var actions: items.filter(item => !item.listed)
  readonly property var rows: items.filter(item => item.listed)
  readonly property int width: level?.width ?? Config.menu.width
  // Why a level's list is empty, in its place; "" while it lists something.
  readonly property string emptyText: level?.entry?.list && rows.length === 0 ? emptyFor(level.entry.list) : ""
  // A calendar level is titled by the month it shows.
  readonly property string title: level?.entry?.view === "calendar" ? Clock.locale.standaloneMonthName(selected.getMonth()) + " " + selected.getFullYear() : level?.title ?? ""
  // The top-level entry the clock opens.
  readonly property var calendar: itemsOf(null).find(item => item.entry.view === "calendar") ?? null
  // A calendar level's keys: each moves the selected day by [years, months, days] per key, or back to today (null).
  // The arrows go unlisted, since the grid shows them; Return focuses the events (README.md#calendar).
  readonly property var calendarKeys: [
    { key: ["Left", "Right"], what: "day", unlisted: true, moves: { Left: [0, 0, -1], Right: [0, 0, 1] } },
    { key: ["Up", "Down"], what: "week", unlisted: true, moves: { Up: [0, 0, -7], Down: [0, 0, 7] } },
    { key: ["Ctrl+Left", "Ctrl+Right"], what: "month", moves: { "Ctrl+Left": [0, -1, 0], "Ctrl+Right": [0, 1, 0] } },
    { key: ["Ctrl+Up", "Ctrl+Down"], what: "year", moves: { "Ctrl+Up": [-1, 0, 0], "Ctrl+Down": [1, 0, 0] } },
    { key: "t", what: "today", clickable: true, moves: { t: null } },
    { key: "Return", what: "events", clickable: true }
  ]
  // With the events in focus, the keys that move through them and back.
  readonly property var eventKeys: [{ key: ["Up", "Down"], what: "event" }, { key: "BackSpace", what: "back", clickable: true }]
  // Keys a level takes beyond its rows, shown under the header (KeyLegend): { key, what }.
  readonly property var legend: level?.entry?.view === "calendar" ? (eventFocus ? eventKeys : calendarKeys.filter(entry => !entry.unlisted))
    : level?.entry?.list === "tray" ? [{ key: "Ctrl+digit", what: "app menu" }]
    : []

  // Selects a day, by key or click, leaving the events.
  function select(day) {
    selected = day;
    eventFocus = "";
  }

  // Focuses an event's row, by key or click, selecting its day.
  function focusEvent(row) {
    eventFocus = row.id;
    selected = row.day;
  }

  // Whether a calendar key was pressed, acting on it if so. Return focuses the events from the selected day on; the arrows
  // then step through them, the day following the event in focus, and Backspace leaves them. Any other key moves the day.
  function calendarKey(name) {
    const rows = Calendar.timeline;
    const at = eventFocus ? rows.findIndex(row => row.id === eventFocus) : -1;
    if (name === "Return") {
      const first = rows.find(row => row.key >= Calendar.key(selected));
      if (first) focusEvent(first);
      return true;
    }
    if (at >= 0 && (name === "Up" || name === "Down")) {
      focusEvent(rows[Math.min(Math.max(at + (name === "Up" ? -1 : 1), 0), rows.length - 1)]);
      return true;
    }
    if (eventFocus && name === "BackSpace") {
      eventFocus = "";
      return true;
    }
    const entry = calendarKeys.find(entry => entry.moves && MenuKeys.matches(entry.key, name));
    if (!entry) return false;
    const move = entry.moves[name];
    if (move) moveSelected(...move);
    else select(new Date());
    return true;
  }

  // Moves the selected day; a month or year away keeps the day, or the month's last when it has fewer.
  function moveSelected(years, months, days) {
    const year = selected.getFullYear() + years;
    const month = selected.getMonth() + months;
    const last = new Date(year, month + 1, 0).getDate();
    select(new Date(year, month, Math.min(selected.getDate(), last) + days));
  }

  function itemsOf(level) {
    if (level === null)
      return entries.map(entry => ({
        key: entry.key,
        desc: entry.desc,
        status: entry.status ?? "",
        icon: entry.icon ?? "",
        enabled: Display.available(entry.requires),
        paired: MenuKeys.isPair(entry),
        entry: entry
      }));
    if (level.kind === "menu") {
      const children = Array.from(menuOpener.children.values).filter(child => !child.isSeparator && child.enabled);
      const labels = children.map(child => child.text.replace(/_/g, ""));
      const keys = MenuKeys.assign(labels);
      return children.map((child, i) => ({
        key: keys[i],
        desc: labels[i],
        glyphs: child.checkState === Qt.Checked ? [Glyphs.check] : child.hasChildren ? [Glyphs.submenu] : [],
        menuEntry: child
      }));
    }

    // An entry's own actions by letter, then what its list holds by digit.
    const actions = Array.from(level.entry.submenu ?? []).map(action => ({
      key: action.key,
      desc: action.desc,
      icon: action.icon ?? "",
      enabled: Display.available(action.requires) && Display.allows(action.layout),
      paired: MenuKeys.isPair(action),
      active: isOn(action.activeWhen) || action.layout === Display.layout,
      listed: /^[0-9]$/.test(action.key),
      entry: action
    }));
    return actions.concat(numbered(level.entry.list, level.entry.pins));
  }

  // Pins first, each keeping its digit, dimmed while what it matches is absent; the rest follow in the list's order.
  function numbered(list, pins) {
    const rest = listed(list);
    const pinned = Array.from(pins ?? []).map(pin => {
      const index = rest.findIndex(item => MenuKeys.glob(pin.match, item.id));
      if (index < 0) return { desc: pin.label, icon: pin.icon ?? listIcon(list), enabled: false };
      const item = rest.splice(index, 1)[0];
      return Object.assign(item, { desc: pin.label, icon: pin.icon ?? item.icon });
    });
    return pinned.concat(rest).slice(0, 9).map((item, i) => Object.assign(item, { key: String(i + 1), listed: true }));
  }

  // The widest key of a group of items, in characters.
  function keyChars(group) {
    return Math.max(1, ...group.map(item => MenuKeys.label(item.key, item.paired).length));
  }

  // Actions flow across, so a column is every nth one.
  function columnChars(index, columns) {
    return keyChars(actions.filter((_, i) => i % columns === index % columns));
  }

  function isOn(state) {
    switch (state) {
    case "wifi": return Network.wifiEnabled;
    case "bluetooth": return Bluez.enabled;
    case "repeat": return Music.repeat;
    case "shuffle": return Music.shuffle;
    case "sink-muted": return Audio.sinkMuted;
    case "source-muted": return Audio.sourceMuted;
    default: return Integrations.get(state)?.active ?? false;
    }
  }

  // Off says nothing here: the header already shows it, and Power is a key away.
  function emptyFor(list) {
    switch (list) {
    case "tray": return "No apps in the tray";
    case "wifi": return Network.wifiEnabled ? "No saved networks in range" : "";
    case "bluetooth": return !Bluez.present ? "No Bluetooth adapter" : Bluez.enabled ? "No paired devices" : "";
    case "sinks": return "No outputs";
    case "sources": return "No inputs";
    default: return "";
    }
  }

  function listIcon(list) {
    switch (list) {
    case "sinks": return Glyphs.audioDevice("external");
    case "sources": return Glyphs.microphone;
    case "wifi": return Glyphs.wifi;
    case "bluetooth": return Glyphs.bluetooth;
    default: return Glyphs.tray;
    }
  }

  function byName(items) {
    return items.sort((a, b) => a.desc.localeCompare(b.desc));
  }

  function audioItems(devices, current) {
    return devices.map(device => ({
      id: device.name,
      desc: device.label,
      icon: Glyphs.audioDevice(device.type),
      active: device.name === current,
      audio: device
    }));
  }

  // What a runtime list holds, without keys; each item's id is what pins match.
  function listed(list) {
    switch (list) {
    case "tray":
      return Tray.items.map(app => ({ id: app.id, desc: app.title || app.id, image: app.icon, app: app }));
    case "wifi":
      return Network.known.map(network => ({ id: network.name, desc: network.name, icon: Glyphs.wifi, active: network.connected, network: network }));
    case "bluetooth":
      return Bluez.enabled ? byName(Bluez.paired.map(device => ({
        id: device.address,
        desc: device.name,
        icon: Glyphs.bluetooth,
        active: device.connected,
        detail: device.connected && device.batteryAvailable ? Math.round(device.battery * 100) + "%" : "",
        device: device
      }))) : [];
    case "sinks":
      return audioItems(Audio.sinks, Audio.sink?.name);
    case "sources":
      return audioItems(Audio.sources, Audio.source?.name);
    default:
      return [];
    }
  }

  function show() {
    // Sources without change events are read as the menu opens.
    Audio.refreshDevices();
    Display.refresh();
    open = true;
  }

  function toggle() {
    if (open) hide();
    else show();
  }

  function toggleEntry(key) {
    const item = find(itemsOf(null), key);
    if (!item) return;
    if (level?.entry === item.entry) {
      hide();
      return;
    }
    stack = [];
    if (!open) show();
    activate(item, key);
  }

  function hide() {
    open = false;
    stack = [];
    shortcutsShown = false;
  }

  // A level starts with no event in focus, coming back to it too.
  function back() {
    stack = stack.slice(0, -1);
    eventFocus = "";
  }

  function push(next) {
    stack = stack.concat([next]);
    eventFocus = "";
  }

  function find(candidates, name) {
    return candidates.find(item => MenuKeys.matches(item.key, name));
  }

  // A pair runs the command of the key pressed; clicking a pair, which names no key, runs nothing.
  function run(entry, name) {
    const cmd = MenuKeys.isPair(entry) ? Array.from(entry.cmd)[MenuKeys.names(entry.key).indexOf(name)] : entry.cmd;
    if (!cmd) return;
    if (!entry.keepOpen) hide();
    Quickshell.execDetached(["sh", "-c", cmd]);
  }

  // An item with a level below opens it, any other runs.
  function activate(item, name) {
    if (item.enabled === false) return;
    if (item.app) {
      hide();
      item.app.activate();
    } else if (item.menuEntry?.hasChildren) {
      push({ kind: "menu", title: item.desc, handle: item.menuEntry, width: level.width });
    } else if (item.menuEntry) {
      // Before hide(), which releases the menu this entry belongs to.
      item.menuEntry.triggered();
      hide();
    } else if (item.audio) {
      Audio.select(item.audio.kind, item.audio.name);
    } else if (item.device || item.network) {
      const target = item.device ?? item.network;
      if (target.connected) target.disconnect();
      else target.connect();
    } else if (item.entry.submenu || item.entry.list || item.entry.view) {
      selected = new Date();
      push({ kind: "entry", title: item.entry.desc, status: item.entry.status, entry: item.entry, width: item.entry.width });
    } else {
      run(item.entry, name);
    }
  }

  // A tray app's own menu: Ctrl with its digit, or a right click.
  function openMenu(item) {
    if (item?.app?.hasMenu) push({ kind: "menu", title: item.desc, handle: item.app.menu, width: level.width });
  }

  // The mouse wheel on a row: a pair runs itself; an entry runs the action of its own on that key, e.g. volume.
  function act(item, name) {
    const action = item.entry?.submenu ? find(Array.from(item.entry.submenu), name) : MenuKeys.matches(item.entry?.key ?? [], name) ? item.entry : null;
    if (action && Display.available(action.requires)) run(action, name);
  }

  // Esc and Backspace are kept free of entries by the assertions in modules/home-manager/programs/desktop-shell.nix.
  function handleKey(event) {
    const name = MenuKeys.name(event);
    // A modifier on its own has no name, and Super belongs to the compositor: a chord with it runs nothing here.
    if (!name || event.modifiers & Qt.MetaModifier) return false;
    const item = find(items, name);
    const listed = find(rows, name.replace(/^Ctrl\+/, ""));
    if (name === "Escape") hide();
    else if (level?.entry?.view === "calendar" && calendarKey(name)) return true;
    else if (name === "BackSpace" && level) back();
    else if (name.startsWith("Ctrl+") && listed) openMenu(listed);
    else if (item) activate(item, name);
    else return false;
    return true;
  }

  QsMenuOpener {
    id: menuOpener
    menu: root.level?.kind === "menu" ? root.level.handle : null
  }
}
