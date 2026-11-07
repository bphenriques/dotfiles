# desktop-shell

A [Quickshell](https://quickshell.org) menu with live status, plus the screen-recording pill. Configured by `modules/home-manager/programs/desktop-shell.nix`, which writes `$XDG_CONFIG_HOME/desktop-shell/config.json`: the shell's only input.

## Layout

- `services/`: state read from its source (D-Bus, Pipewire, niri, files), pushed by events. Sources that cost something stay closed until a `Subscription { service: X }` exists.
- `ui/`: the kit below. Widgets use it and the theme from `Config`, never raw colours or sizes.
- `widgets/`: the surfaces. `menu/` splits into `MenuModel` (state, keys) and `Menu`, `MenuStatus` (view).

## Menu

wlr-which-key's grammar: every key visible, in columns, as `key ➜ status`. A letter opens a level or runs a command; Esc closes, Backspace goes back. A level can also be filled at runtime (`list = "tray"`, an app's own menu).

## Kit

| Component | Use |
|---|---|
| `Label` | all text; `tone` is `normal`, `dim`, `primary`, `warning` or `alert` |
| `Icon` | a glyph or an image in the same square |
| `StatusLine` | icon, text, and a dim detail on the right |
| `KeyHint` | `key ➜` |
| `ListItem` | a row with hover, click, right click and wheel |
| `Surface`, `Pill` | a panel; a capsule outlined in its tone |
| `OverlayWindow`, `DismissArea` | a popup above everything; the click-outside catcher under it |
| `Glyphs` | the icon set; glyphs other programs own arrive through `Config.glyphs` |

`alert` is only for what needs attention: critical battery, recording, do-not-disturb.
