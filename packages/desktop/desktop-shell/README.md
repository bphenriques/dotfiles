# desktop-shell

A [Quickshell](https://quickshell.org) menu with live status, a pop-up for volume, brightness, keyboard-layout and keep-awake changes, a workspace minimap when where you are changes, and the screen-recording pill. Configured by `modules/home-manager/programs/desktop-shell.nix`, which writes `$XDG_CONFIG_HOME/desktop-shell/config.json`: the shell's only input. `Config.qml` holds the defaults; the module only passes settings through.

## Design rules

Every change follows these; a change that cannot is a reason to revisit the rule here first, not to work around it.

1. **Data, not code.** What the menu shows and does comes from `menu.entries`. QML has no case for a particular entry, tool or device; a new need becomes a schema field in the module, with an assertion when it can be wrong.
2. **Tools through `Config.commands`.** QML runs nothing by name. The module that owns a tool sets its command, and the shell reads only output it defines (a profile name, `true`/`false` lines), never a tool's own format. niri is the one exception.
3. **Events over polling.** What has no events is read when the menu opens; what must sample (the mic level) runs only while shown.
4. **Layout is declared.** A level's columns and width come from its entry (`columns`, `width`, else `menu.columns`, `menu.width`), never measured from what it shows, so it holds still; entries flow across, so order decides neighbours. Make the menu wider before adding columns or truncating.
5. **Order is explicit.** Top level by `lib.mkOrder`: 100 audio, 300 network, 400 Bluetooth, 500 display, 600 wallpaper, 700 music, 800 tray, 2000 session. Within a level, the list order, paired for the row it shares.
6. **Labels.** Sentence case; `...` when it opens a picker or an app; a pair named for what it changes (`Volume`, `Track`). Device levels put Power on `p` and Configure... on `c`; `space` is only ever a fuzzel picker. What ends the session takes Shift (Reboot, Shutdown).
7. **Keys hold still.** Letters are actions, digits are list items, Ctrl+digit an item's own menu, which keeps letters from the labels its app supplies. Modifiers other than Shift run nothing else. Pinned items keep their digit and dim while absent, and their label names them wherever the menu shows them; the rest follow by name (internal audio first), never by signal or recency.
8. **State is visible.** The menu shows state, the OSD shows changes; a level that lists its items shows only its state in the header. A listed item in use and a toggle that is on (`activeWhen`) take the `primary` tone; the right of a row holds data (battery, signal), never a word for that state.
9. **Text is plain.** Everything renders through `Label`, which never interprets markup: titles from apps and streams are untrusted.
10. **Done means seen working.** A change is tested where it runs (a fixture, a nested niri, keys sent with `wtype`), not only evaluated.
11. **Widgets compose the kit.** Colour and dimming come from a `tone`, glyphs go through `Icon`, the wheel through `WheelSteps`; a widget never sets opacity to dim, picks a theme colour itself, or draws a glyph as text.

## Layout

- `services/`: state read from its source (D-Bus, Pipewire, niri, files), pushed by events. Sources without events (kanshi, volume-osd's device type) are read on `refresh()`, which the menu calls as it opens, through `CommandReader`.
- `ui/`: the kit below. Widgets use it and the theme from `Config`, never raw colours or sizes.
- `widgets/`: the surfaces. `menu/` splits into `MenuModel` (state, keys) and `Menu`, `MenuStatus` (view).

## Menu

Every key visible, then its status, labels starting together after a gutter as wide as the longest key in their column. A letter opens a level or runs a command; Esc closes, Backspace goes back. A key pair (`[ "Down" "Up" ]` with one command each) shares a row. A level can also list things found at runtime under its actions, numbered 1 to 9: `list = "tray"`, `"bluetooth"`, `"wifi"`, `"sinks"` or `"sources"`, with `pins` for items that keep their digit. `requires.connected`, `requires.disconnected` and `requires.enabled` dim an entry until the outputs it needs are plugged in, unplugged, or on. `menu toggleEntry <key>` over IPC opens a top-level entry directly, for a compositor shortcut; Backspace still leads to the top level.

## Kit

| Component | Use |
|---|---|
| `Label` | all text, coloured by `tone`: `normal`, `dim`, `primary`, `warning` or `alert` |
| `Tone` | the colour of each tone; `dim` is the normal colour at reduced alpha |
| `Icon` | a glyph or an image in the same square |
| `StatusLine` | icon, text, and a dim detail on the right |
| `KeyHint` | a key in its gutter |
| `Meter` | a thin level bar |
| `Divider` | a line between two groups of rows |
| `ListItem` | a row with hover, click, right click and wheel; dims itself when disabled |
| `WheelSteps` | a pointer area that turns the wheel into one step per notch |
| `Surface`, `Pill` | a panel; a capsule outlined in its tone |
| `OverlayWindow`, `DismissArea` | a popup above everything; the click-outside catcher under it |
| `Glyphs` | the icon set; glyphs other programs own arrive through `Config.glyphs` |

`alert` is only for what needs attention: critical battery, recording, do-not-disturb.

## Runtime

Nothing runs from PATH: each external command arrives through `Config.commands`, set by the module that owns the tool (volume-osd, dunst, kanshi, screen-recorder), so swapping a tool means changing its module, not the shell. One left unset shows as an empty status. niri is the exception: the shell follows its event stream directly.
