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
  // Levels opened below the top one, the last is shown: { kind, title, status, entry, handle }.
  property var stack: []

  readonly property var entries: Array.from(Config.menu.entries)
  readonly property var level: stack[stack.length - 1] ?? null
  readonly property var items: itemsOf(level)

  function itemsOf(level) {
    if (level === null)
      return entries.map(entry => ({ key: entry.key, desc: entry.desc, status: entry.status ?? "", icon: entry.icon ?? "", entry: entry }));
    switch (level.kind) {
    case "submenu":
      return Array.from(level.entry.submenu).map(action => ({ key: action.key, desc: action.desc, entry: action }));
    case "tray": {
      const keys = WhichKey.assign(Tray.items.map(app => app.title || app.id));
      return Tray.items.map((app, i) => ({ key: keys[i], desc: app.title || app.id, image: app.icon, detail: app.hasMenu ? "⇧ menu" : "", app: app }));
    }
    case "menu": {
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
    }
    return [];
  }

  function toggle() {
    if (open) {
      hide();
    } else {
      stack = [];
      open = true;
    }
  }

  function hide() {
    open = false;
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

  function run(entry) {
    if (!entry.keepOpen) hide();
    Quickshell.execDetached(["sh", "-c", entry.cmd]);
  }

  // Like wlr-which-key: an item with a level below opens it, any other runs.
  function activate(item) {
    if (item.app) {
      hide();
      item.app.activate();
    } else if (item.menuEntry?.hasChildren) {
      push({ kind: "menu", title: item.desc, handle: item.menuEntry });
    } else if (item.menuEntry) {
      hide();
      item.menuEntry.triggered();
    } else if (item.entry.submenu) {
      push({ kind: "submenu", title: item.entry.desc, status: item.entry.status, entry: item.entry });
    } else if (item.entry.list) {
      push({ kind: item.entry.list, title: item.entry.desc, status: item.entry.status });
    } else {
      run(item.entry);
    }
  }

  // A tray app's own menu: Shift with its letter, or a right click.
  function openMenu(item) {
    if (item?.app?.hasMenu) push({ kind: "menu", title: item.desc, handle: item.app.menu });
  }

  // Runs one of an entry's actions without opening it: volume from the mouse wheel.
  function act(item, name) {
    const action = find(Array.from(item.entry?.submenu ?? []), name);
    if (action) run(action);
  }

  // Esc and Backspace are kept free of entries by the assertions in modules/home-manager/programs/desktop-shell.nix.
  function handleKey(event) {
    const name = WhichKey.name(event);
    const item = find(items, name);
    if (name === "Escape") hide();
    else if (name === "BackSpace" && level) back();
    else if (item) activate(item);
    else if (level?.kind === "tray" && name.length === 1 && name !== name.toLowerCase()) openMenu(find(items, name.toLowerCase()));
    else return false;
    return true;
  }

  QsMenuOpener {
    id: menuOpener
    menu: root.level?.kind === "menu" ? root.level.handle : null
  }
}
