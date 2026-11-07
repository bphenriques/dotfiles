.pragma library

// Keys by their xkb names (a, A, Up, space, ...), as menu entries declare them.

// The name of a key event, ignoring Ctrl.
function name(event) {
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
  if (event.key >= Qt.Key_A && event.key <= Qt.Key_Z) {
    const letter = String.fromCharCode(event.key);
    return event.modifiers & Qt.ShiftModifier ? letter : letter.toLowerCase();
  }
  return event.text;
}

// Whether an entry key (a name or a list of names) matches a key name.
function matches(key, name) {
  return typeof key === "string" ? key === name : Array.from(key).includes(name);
}

// Short label for an entry key: its first name, with arrows as symbols.
function label(key) {
  const first = typeof key === "string" ? key : key[0];
  return { Up: "↑", Down: "↓", Left: "←", Right: "→", space: "Space", Return: "Enter" }[first] ?? first;
}

// One key per label for entries known only at runtime: each label's first letter not taken yet, else a digit.
function assign(labels) {
  const taken = new Set();
  return labels.map(label => {
    const letters = String(label).toLowerCase().replace(/[^a-z]/g, "") + "0123456789";
    const key = Array.from(letters).find(letter => !taken.has(letter)) ?? "";
    taken.add(key);
    return key;
  });
}
