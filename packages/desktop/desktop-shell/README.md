# desktop-shell

A [Quickshell](https://quickshell.org) menu with live status and a calendar, and a pop-up for volume, brightness, keyboard-layout and keep-awake changes; optional integrations add the rest (a workspace minimap, the screen-recording pill, the wallpaper's name). Configured by `modules/home-manager/programs/desktop-shell.nix`, which writes `$XDG_CONFIG_HOME/desktop-shell/config.json`: the shell's only input. `Config.qml` holds the defaults; the module only passes settings through. Settings are grouped by feature, like waybar's modules (`clock`, `audio`, `notifications`, ...); one the shell does not know is logged at startup and ignored.

## Design rules

Every change follows these; a change that cannot is a reason to revisit the rule here first, not to work around it.

1. **Data, not code.** What the menu shows and does comes from `menu.entries`. QML has no case for a particular entry, tool or device; a new need becomes a schema field in the module, with an assertion when it can be wrong.
2. **Tools through the config.** QML runs nothing by name. The module that owns a tool sets its command, and the shell reads only output it defines (a layout name, `true`/`false` lines), never a tool's own format. What needs more of one tool is an integration. The compositor is reached only through `Compositor` (see Compositor).
3. **Events over polling.** What has no events is read when the menu opens; what must sample (the mic level) runs only while shown.
4. **Layout is declared.** A level's columns and width come from its entry (`columns`, `width`, else `menu.columns`, `menu.width`), never measured from what it shows, so it holds still; entries flow across, so order decides neighbours. Make the menu wider before adding columns or truncating.
5. **Order is explicit.** Top level by `lib.mkOrder`: 100 audio, 300 network, 400 Bluetooth, 500 display, 600 wallpaper, 700 music, 800 tray, 900 calendar, 2000 session. Within a level, the list order, paired for the row it shares.
6. **Labels.** Sentence case; `...` when it opens a picker or an app; a pair named for what it changes (`Volume`, `Track`). Device levels put Power on `p` and Configure... on `c`; every action in a level carries an icon; `space` is only ever a fuzzel picker. What ends the session takes Shift (Reboot, Shutdown).
7. **Keys hold still.** Letters are actions, digits are list items, Ctrl+digit an item's own menu, which keeps letters from the labels its app supplies. A key may carry Ctrl or Alt in its name (`Ctrl+Left`, as niri writes bindings); hints show keys Emacs-style, `C-` Ctrl, `M-` Alt, `s-` Super, Shift as the capital letter (`C-←→`, `s-k`), symbols for the arrows, `DEL` for Backspace as Emacs writes it; a modifier never runs the plain key, Shift only picks the capital letter, and Super stays the compositor's. Pinned items keep their digit and dim while absent, and their label names them wherever the menu shows them; the rest follow by name (internal audio first), never by signal or recency.
8. **State is visible.** The menu shows state, the OSD shows changes; a level that lists its items shows only its state in the header. A listed item in use and a toggle that is on (`activeWhen`) take the `primary` tone; the right of a row holds data (battery, signal), never a word for that state. A top-level status names its state only when it has no value to show (`off`, `muted`, `paused`). What cannot run right now dims, so keys hold still; what can never run on this machine is not declared.
9. **Text is plain.** Everything renders through `Label`, which never interprets markup: titles from apps and streams are untrusted. The one source-specific case in the core is next to it: Music reads song and artist out of the XML some radio streams (Bauer Media) send as their title, since MPRIS leaves no glue to do it in.
10. **Done means seen working.** A change is tested where it runs (see Testing), not only evaluated.
11. **Widgets compose the kit.** Colour and dimming come from a `tone`, glyphs go through `Icon`, the wheel through `WheelSteps`, a colour that is data (a calendar's) through `Swatch`, a size in characters or lines through `Metrics`; a widget never sets opacity to dim, picks a theme colour itself, or draws a glyph as text.
12. **The core is what every desktop has.** Audio, network, Bluetooth, power, display, tray, notifications, music, the calendar. Anything tied to one tool or taste is an integration: off until listed, reaching the shell only through its API (see Integrations), and held to these rules like the core, keyboard first and built from the kit.
13. **Motion confirms, never delays.** Surfaces fade in and out, colours ease, all over `theme.animationDuration` (`0` for none) and through the kit (`Fade`, `Label`, `Icon`, `ListItem`), never a widget's own. Nothing waits on an animation: a key acts at once, the menu appears and closes at once (its background blur cannot fade with it, and a fade-out would hold the keyboard), and nothing moves or resizes.

## Layout

- `services/`: state read from its source (D-Bus, Pipewire, niri, commands), pushed by events. What a service declares at its top level is its API, read-only where it is state; what it keeps for itself sits in an object under an `id`, which no other file can reach. Sources without events (the outputs, the audio device list, the display layout) are read on `refresh()`, which the menu calls as it opens.
- `ui/`: the kit below. Widgets use it and the theme from `Config`, never raw colours or sizes.
- `widgets/`: the surfaces. `menu/` splits into `MenuModel` (state, keys) and `Menu`, `MenuStatus` (view).
- `integrations/`: the integrations the shell ships; `Integration.qml` is their root, `services/Integrations.qml` loads them.
- `calendar-sync/`: its own package, which writes `calendar.file` from a list of calendars (vdirsyncer, khal); the shell never runs it.

## Menu

Every key visible, then its status, labels starting together after a gutter as wide as the longest key in their column. A letter opens a level or runs a command; Esc closes, Backspace goes back. A key pair (`[ "Down" "Up" ]` with one command each) shares a row. A level can also list things found at runtime under its actions, numbered 1 to 9: `list = "tray"`, `"bluetooth"`, `"wifi"`, `"sinks"` or `"sources"`, with `pins` for items that keep their digit. A list with nothing in it says why in its place (`No paired devices`, `No saved networks in range`), unless the header already does (off). `requires.connected` and `requires.enabled` dim an entry until the outputs it needs are plugged in or on. `layout` lights an entry while the screens are in that layout, and dims it while they cannot be. A level that takes keys beyond its rows lists them on one line under its header (`KeyLegend`), with the level's actions on the right of it, from the same list that handles them: the calendar's, and Ctrl+digit for a tray app's menu. `menu toggleEntry <key>` over IPC opens a top-level entry directly, for a compositor shortcut; Backspace still leads to the top level.

The header is `menu.header.left`, `center` and `right`, like waybar's modules: built-in items (`clock`, `notifications`, `keepAwake`, `keyboardLayout`, `battery`) and integrations, by name. The centre is as wide as its items and centred, the sides share the rest, so the menu holds still; crowded, text shortens with an ellipsis, the centre's first, while icons and short values (`80%`, `EN`) stay whole. Inside a level the left holds its title, the right its state. The clock takes `clock.format`, strftime as waybar's (`%A, %-d%o %B %H:%M`, `%o` the English ordinal), else the locale's long date; `clock.locale` (`en_GB`) names days and months for the clock and the calendar, else the system's.

The mouse reaches what the keys do, nothing more: a click runs a row, a right click opens a tray app's menu, the wheel steps a row's pair (volume, brightness) or the calendar's month, a click on a day selects it, the title goes back (at the top, the clock opens the calendar), and a click on a header item flips it (notifications, keep awake, keyboard layout) or calls an integration's `activate()`. Holding Super for a second shows their compositor keys (each feature's `shortcut`, set by the module that binds it, or an integration's).

## Calendar

`view = "calendar"` gives a level a month under its entries, titled by the month, weeks starting on `clock.firstDayOfWeek` (the locale's by default). The arrows move the selected day by a day or a week, unlisted since the grid shows them; the legend line has the rest: `Ctrl+←→` a month, `Ctrl+↑↓` a year, `t` back to today, and `Return` into the events.

Once `calendar.file` exists, the month shows events: a dot per colour under each day that has them (four at most), and beside it, as tall as the month, one list of every event under a heading per day, starting at the selected day (`No events` when it has none). The month is how you move through the list, as in Fantastical: `Return` focuses the events from the selected day on; the arrows then step through them, the selected day following, and Backspace or any other calendar key returns to the month. A click on an event focuses it. Events are all-day first, then by start: a bar in its colour, start over end, then title over location, one line when neither has a second; a timed event that fills the day reads as all-day. The `calendar` status shows the next event on a row, timed ones first so a daily all-day event does not hold it, and in the level's header how old the events are (`updated 08:30`, or `syncing…`); a failed sync says why on its own line under the legend. With the events in focus, the legend offers `↑↓ event` and `DEL back`.

The shell only reads the file, watching it for changes; the module that owns the calendar tool writes it, replacing it whole:

```json
{
  "updated": "2026-11-12T14:30",
  "error": "personal: could not sync",
  "events": [
    { "title": "Dentist", "start": "2026-11-12T14:00", "end": "2026-11-12T15:00", "allDay": false, "color": "#4285f4", "location": "Rua Augusta 120" },
    { "title": "Trip", "start": "2026-11-20", "end": "2026-11-22", "allDay": true, "color": "#33b679" }
  ]
}
```

Times are local, without an offset; an all-day event's `start` and `end` are dates, `end` its last day. A repeating event is one entry per occurrence. `location` is optional, and line breaks in it or the title read as commas. `error` says why the last sync failed, over the events it kept, and `syncing: true` marks one under way; both are left out otherwise. `calendar.remind`, given an event's title and when, reminds of each timed event once, `calendar.remindBefore` minutes ahead (10). Events are shown, never acted on. `calendar-sync` covers 62 days back and a year ahead, so months beyond show none.

## Kit

What the core's surfaces and the integrations are built from; an integration that needs something new adds it here first.

| Component | Use |
|---|---|
| `Label` | all text, coloured by `tone`: `normal`, `dim`, `primary`, `warning` or `alert` |
| `Tone` | the colour of each tone, `dim` being the normal colour at reduced alpha; a panel's background; a tone washed behind something (a highlight, a track) |
| `Metrics` | the font's character width and line height, for sizes given in characters or lines |
| `Icon` | a glyph or an image in the same square |
| `StatusLine` | icon, text, and a dim detail on the right, glyphs before it drawn as icons (a mode, a submenu) |
| `KeyHint` | a key in its gutter |
| `KeyLegend` | keys a level takes beyond its rows, on one line under the header: each key as `KeyHint` shows it, then what it does |
| `Meter` | a thin level bar |
| `Divider` | a line between two groups of rows |
| `ListItem` | a row with hover, click, right click and wheel, highlighted while `current` (a selected day, an event in focus); dims itself when disabled |
| `WheelSteps` | a pointer area that turns the wheel into one step per notch |
| `Surface`, `Pill` | a panel; a capsule outlined in its tone |
| `Swatch` | a dot, or a bar when stretched, in a colour that is data (a calendar's), dimmed by its tone like the rest |
| `Shadow` | a shadow like niri's default, for a surface inside a larger window |
| `OverlayWindow`, `DismissArea` | a popup above everything; the click-outside catcher under it |
| `Transient` | shown for a moment on `show()`, held while `held`, ignoring startup's replayed state until `armed` |
| `Fade` | fades a surface in and out over `theme.animationDuration`; `loaded` holds a `LazyLoader` until it has faded out |
| `MenuKeys` | key names as entries declare them (`Ctrl+Left`), their hints, and keys for an app's own menu |
| `Glyphs` | the icon set, Material Design from Nerd Fonts, each an escape (`"\u{F05A9}"`) named after its entry; glyphs other programs own arrive in their feature's `glyphs` (`audio.glyphs`) |

`alert` is only for what needs attention: critical battery, recording, do-not-disturb, a failed calendar sync.

## Integrations

An integration is one QML file whose root is `Integration`, listed by name under `integrations` in `config.json` as `{ source, settings }`; the module's `integrations.<name>` writes it, set by the module that owns the tool. `source` is a path, so your own can live anywhere, or relative to the shell; left out, it is the one shipped under that name (`workspace-map`: `integrations/WorkspaceMap.qml`). Nothing loads unlisted; one that fails to load is logged and left out, and the shell runs on.

Its API, and the only one:

- In: `name`, and `settings`, its own part of the config; it holds the defaults (`settings.height ?? 30`).
- Out: `glyph`, `text` and `detail`, shown by a menu entry whose `status` is its name and in the header when `menu.header` names it; `active`, which lights either; `activate()`, what a click on its header item does; and `shortcut`, its compositor key, shown while Super is held. The module rejects either naming an integration not listed, and an integration taking a built-in name.
- Uses: the theme (`Config.theme`, `Config.popups`), the kit, and the services' APIs (`Audio`, `Network`, `Bluez`, `Power`, `Display`, `Brightness`, `Music`, `Tray`, `Startup`, `Compositor`), plus a compositor's own backend when it is built on one (`Niri`, whose `received` passes on every event).
- Actions: an `IpcHandler` targeting its name, which menu entries and compositor keys call through `exec.ipc`.

| Shipped | Shows | Settings |
|---|---|---|
| `Recording.qml` | a pill on every screen while recording, a click stops it; status: the time, lit while recording | `file` (under `$XDG_RUNTIME_DIR`), `stop` |
| `Wallpaper.qml` | status: the wallpaper's name | `file` (under `$XDG_RUNTIME_DIR`) |
| `WorkspaceMap.qml` | a minimap of niri's workspaces as you move between windows or workspaces | `height`, `maxWidth`, `opacity`, `borderWidth` |

## Compositor

The core asks the compositor for two things, through `Compositor`, and nothing more:

- The outputs connected and the outputs on, each by connector name and by make, model and serial (what `requires` and `display.panel` name).
- The keyboard layouts, the current one, and switching to the next.

A backend per compositor answers them, under `services/compositor/`, picked at startup from the environment (`NIRI_SOCKET` today; another compositor would add its own, such as `HYPRLAND_INSTANCE_SIGNATURE`). Without one, the outputs come from Wayland, which only knows those on, so `requires.connected` reads as `requires.enabled`, and the layout status stays empty. A backend ships once it is seen working (rule 10): niri today.

What only one compositor has stays with its backend (`Niri`: workspaces, windows, every event), for integrations built on it; such an integration names its compositor (the workspace map is niri's). What the compositor is told (keys, layer rules, outputs) lives in the Nix modules, never in the shell.

## Runtime

Nothing runs from PATH: each external command arrives in its feature's settings (`audio.devices`, `notifications.toggle`) or an integration's, set by the module that owns the tool, so swapping a tool means changing its module, not the shell. One left unset shows as an empty status. The compositor is the exception: its backend talks to it directly, niri's through its socket.

## Testing

`check.sh` holds the rules above that a text search can (an integration's root and imports, Compositor, Label, glyphs through Icon, colours only through tones and the kit, font measures through Metrics, commands from the config); every build runs it.

`nix run .#desktop-shell-preview -- <shell dir> [config.json]` runs a working tree in a nested niri, with your `config.json` or a fixture (the built one edited with `jq`), and opens a shell inside it with `wtype`, `wlrctl` and `grim` at hand. Exit that shell to stop it. Session actions run for real, so never press them there. `preview/events.json` is a calendar to point `calendar.file` at, around 12 November 2026.

Screenshots come from the live desktop after a switch, not the preview (it only draws while its window is visible): open a level with `qs -c desktop-shell ipc call menu toggleEntry <key>` (`menu toggle` for the top level), wait a second, `grim <file>`, then close it with `menu toggle`. A pop-up shows on its own change (`niri msg action switch-layout next`, `qs -c desktop-shell ipc call idle toggle`, then the same again to undo). Anything opened inside the preview runs its `$SHELL`, so never point that at a script.
