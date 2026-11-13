# The design rules a text search can hold (README.md), run on every build from the shell's root.
failed=0
fail() {
  printf '%s\n' "$1" >&2
  failed=1
}

# Rule 12: an integration is one file rooted at Integration, using the theme, the kit and the services, never the core's surfaces.
for file in integrations/*.qml; do
  grep -q '^Integration {' "$file" || fail "$file: root is not Integration"
  grep -q '^import qs\.widgets' "$file" && fail "$file: imports the core's surfaces"
done

# Compositor: only Compositor and integrations reach a backend.
for file in $(grep -rlE '\bNiri\.' --include='*.qml' . | grep -vE '^\./(services/Compositor\.qml|services/compositor/|integrations/)'); do
  fail "$file: reaches niri past Compositor"
done

# Rule 9: text renders through Label.
for file in $(grep -rlE '^\s*Text \{' --include='*.qml' . | grep -v '^\./ui/Label\.qml$'); do
  fail "$file: Text outside Label"
done

# Rule 11: glyphs go through Icon, never into text or a detail.
for file in $(grep -rlE '(text|detail|fallbackDetail): .*Glyphs\.' --include='*.qml' . | grep -v '^\./ui/Icon\.qml$'); do
  fail "$file: a glyph drawn as text"
done

# Rule 11: a widget never picks a theme colour itself; tones and the kit do.
for file in $(grep -rlE 'Config\.theme\.(primary|foreground|background|alert|warning)\b' --include='*.qml' . | grep -vE '^\./(Config\.qml|ui/)'); do
  fail "$file: picks a theme colour itself"
done

# Rule 11: sizes in characters or lines come from Metrics.
for file in $(grep -rlE '^\s*(TextMetrics|FontMetrics) \{' --include='*.qml' . | grep -v '^\./ui/Metrics\.qml$'); do
  fail "$file: measures the font itself"
done

# Rule 11: colours come from the theme.
for file in $(grep -rlE '"#[0-9a-fA-F]{6,8}"' --include='*.qml' . | grep -vE '^\./(Config\.qml|ui/)'); do
  fail "$file: a colour outside the theme"
done

# Rule 2: commands come from the config, run with sh -c, never named in QML.
for file in $(grep -rlE '(execDetached\(|command: |command = )\[ *"[^s"]|"sh", "-c", "' --include='*.qml' .); do
  fail "$file: runs a command named in QML"
done

exit "$failed"
