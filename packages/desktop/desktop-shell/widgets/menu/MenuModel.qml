import QtQuick
import Quickshell
import qs
import qs.services
import qs.ui
import "../../ui/WhichKey.js" as WhichKey

// The menu's state: whether it is open, the levels opened, and what keys do.
// Every level, static or filled at runtime (tray apps, an app's own menu), is a list of items { key, desc, ... }.
Scope {
  id: root

  property bool open: false
  // Levels opened below the top one, the last is shown: an entry's (kind "entry") or an app's own menu (kind "menu").
  property var stack: []

  readonly property var entries: Array.from(Config.menu.entries)
  readonly property var level: stack[stack.length - 1] ?? null
  readonly property var items: itemsOf(level)
  readonly property var actions: items.filter(item => !item.listed)
  readonly property var rows: items.filter(item => item.listed)
  readonly property int width: level?.width ?? Config.menu.width

  function itemsOf(level) {
    if (level === null)
      return entries.map(entry => ({ key: entry.key, desc: entry.desc, status: entry.status ?? "", icon: entry.icon ?? "", enabled: Display.available(entry.requires), paired: WhichKey.isPair(entry), entry: entry }));
    if (level.kind === "menu") {
      const children = Array.from(menuOpener.children.values).filter(child => !child.isSeparator && child.enabled);
      const labels = children.map(child => child.text.replace(/_/g, ""));
      const keys = WhichKey.assign(labels);
      return children.map((child, i) => ({
        key: keys[i],
        desc: labels[i],
        detail: child.checkState === Qt.Checked ? Glyphs.check : child.hasChildren ? "›" : "",
        menuEntry: child
      }));
    }

    // An entry's own actions by letter, then what its list holds by digit.
    const actions = Array.from(level.entry.submenu ?? []).map(action => ({ key: action.key, desc: action.desc, enabled: Display.available(action.requires), paired: WhichKey.isPair(action), active: isOn(action.activeWhen) || action.profile === Display.profile, listed: /^[0-9]$/.test(action.key), entry: action }));
    return actions.concat(numbered(level.entry.list, level.entry.pins));
  }

  // Pins first, each keeping its digit, dimmed while what it matches is absent; the rest follow in the list's order.
  function numbered(list, pins) {
    const rest = listed(list);
    const pinned = Array.from(pins ?? []).map(pin => {
      const index = rest.findIndex(item => WhichKey.glob(pin.match, item.id));
      if (index < 0) return { desc: pin.label, icon: pin.icon ?? listIcon(list), enabled: false };
      const item = rest.splice(index, 1)[0];
      return Object.assign(item, { desc: pin.label, icon: pin.icon ?? item.icon });
    });
    return pinned.concat(rest).slice(0, 9).map((item, i) => Object.assign(item, { key: String(i + 1), listed: true }));
  }

  // The widest key of a group of items, in characters.
  function keyChars(group) {
    return Math.max(1, ...group.map(item => WhichKey.label(item.key, item.paired).length));
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
    default: return false;
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
      icon: device.kind === "sink" ? Glyphs.audioDevice(device.type) : Glyphs.microphone,
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

  function toggle() {
    if (open) {
      hide();
    } else {
      // Sources without change events are read as the menu opens.
      Audio.refresh();
      Audio.refreshDevices();
      Display.refresh();
      open = true;
    }
  }

  function hide() {
    open = false;
    stack = [];
  }

  function back() {
    stack = stack.slice(0, -1);
  }

  function push(next) {
    stack = stack.concat([next]);
  }

  function find(candidates, name) {
    return candidates.find(item => WhichKey.matches(item.key, name));
  }

  // A pair runs the command of the key pressed; clicking a pair, which names no key, runs nothing.
  function run(entry, name) {
    const cmd = WhichKey.isPair(entry) ? Array.from(entry.cmd)[WhichKey.names(entry.key).indexOf(name)] : entry.cmd;
    if (!cmd) return;
    if (!entry.keepOpen) hide();
    Quickshell.execDetached(["sh", "-c", cmd]);
  }

  // Like wlr-which-key: an item with a level below opens it, any other runs.
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
    } else if (item.entry.submenu || item.entry.list) {
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
    const action = item.entry?.submenu ? find(Array.from(item.entry.submenu), name) : WhichKey.matches(item.entry?.key ?? [], name) ? item.entry : null;
    if (action && Display.available(action.requires)) run(action, name);
  }

  // Esc and Backspace are kept free of entries by the assertions in modules/home-manager/programs/desktop-shell.nix.
  function handleKey(event) {
    const name = WhichKey.name(event);
    const item = find(items, name);
    if (name === "Escape") hide();
    else if (name === "BackSpace" && level) back();
    else if (item?.listed && event.modifiers & Qt.ControlModifier) openMenu(item);
    else if (item && !(event.modifiers & (Qt.ControlModifier | Qt.AltModifier | Qt.MetaModifier))) activate(item, name);
    else return false;
    return true;
  }

  QsMenuOpener {
    id: menuOpener
    menu: root.level?.kind === "menu" ? root.level.handle : null
  }
}
