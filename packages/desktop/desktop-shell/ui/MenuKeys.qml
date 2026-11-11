pragma Singleton

import QtQuick
import Quickshell

// Keys by their xkb names (a, A, Up, space, ...), as menu entries declare them.
Singleton {
  id: root

  // The name of a key event, Ctrl and Alt first when held (Ctrl+Left); Shift only picks the capital letter. "" for a modifier alone.
  function name(event) {
    const key = keyName(event);
    if (!key) return "";
    return (event.modifiers & Qt.ControlModifier ? "Ctrl+" : "") + (event.modifiers & Qt.AltModifier ? "Alt+" : "") + key;
  }

  function keyName(event) {
    switch (event.key) {
    case Qt.Key_Up: return "Up";
    case Qt.Key_Down: return "Down";
    case Qt.Key_Left: return "Left";
    case Qt.Key_Right: return "Right";
    case Qt.Key_Space: return "space";
    case Qt.Key_Return:
    case Qt.Key_Enter: return "Return";
    case Qt.Key_Escape: return "Escape";
    case Qt.Key_Backspace: return "BackSpace";
    }
    if (event.key >= Qt.Key_0 && event.key <= Qt.Key_9) return String.fromCharCode(event.key);
    if (event.key >= Qt.Key_A && event.key <= Qt.Key_Z) {
      const letter = String.fromCharCode(event.key);
      return event.modifiers & Qt.ShiftModifier ? letter : letter.toLowerCase();
    }
    // Held with Ctrl, a key's text is a control character.
    return event.modifiers & Qt.ControlModifier ? "" : event.text;
  }

  // The names an entry key stands for: a single name or a list of them.
  function names(key) {
    return typeof key === "string" ? [key] : Array.from(key);
  }

  // Whether an entry runs one command per key, shown as one row (e.g. Down and Up). Lists from config.json are
  // list-like objects, not JavaScript arrays, so Array.isArray cannot tell.
  function isPair(entry) {
    return entry?.cmd != null && typeof entry.cmd !== "string";
  }

  // Whether an entry key matches a key name.
  function matches(key, name) {
    return root.names(key).includes(name);
  }

  // Short label for an entry key: its first name, or every name for a pair with their shared modifiers once (Ctrl+←→), arrows as symbols.
  function label(key, paired = false) {
    const symbols = { Up: "↑", Down: "↓", Left: "←", Right: "→", space: "SPC", Return: "RET" };
    const names = paired ? root.names(key) : root.names(key).slice(0, 1);
    const modifiers = names[0].match(/^((Ctrl|Alt)\+)*/)[0];
    const shared = names.every(name => name.startsWith(modifiers)) ? modifiers : "";
    return shared + names.map(name => name.slice(shared.length)).map(name => symbols[name] ?? name).join("");
  }

  // Whether text matches a pattern where * stands for anything.
  function glob(pattern, text) {
    const escaped = pattern.replace(/[.+?^${}()|[\]\\]/g, "\\$&").replace(/\*/g, ".*");
    return new RegExp("^" + escaped + "$").test(text);
  }

  // One key per entry of an app's own menu: each label's first letter not taken yet, else a digit.
  function assign(labels) {
    const taken = new Set();
    return labels.map(label => {
      const letters = String(label).toLowerCase().replace(/[^a-z]/g, "") + "0123456789";
      const key = Array.from(letters).find(letter => !taken.has(letter)) ?? "";
      taken.add(key);
      return key;
    });
  }
}
