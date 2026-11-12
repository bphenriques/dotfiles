pragma Singleton

import QtQuick
import Quickshell
import Quickshell.Io

// Typed view of $XDG_CONFIG_HOME/desktop-shell/config.json, written by modules/home-manager/programs/desktop-shell.nix.
// Grouped by feature, like waybar's modules; commands are run with sh -c, and one left unset shows as an empty status.
Singleton {
  id: root

  readonly property alias theme: adapter.theme
  readonly property alias menu: adapter.menu
  readonly property alias clock: adapter.clock
  readonly property alias calendar: adapter.calendar
  readonly property alias popups: adapter.popups
  readonly property alias audio: adapter.audio
  readonly property alias brightness: adapter.brightness
  readonly property alias display: adapter.display
  readonly property alias notifications: adapter.notifications
  readonly property alias keepAwake: adapter.keepAwake
  readonly property alias keyboardLayout: adapter.keyboardLayout
  readonly property alias battery: adapter.battery
  readonly property alias music: adapter.music
  readonly property alias integrations: adapter.integrations

  // Keys of json that object does not declare, by path; free-form values (lists, maps) are not looked into.
  function unknownKeys(json, object, path) {
    let found = [];
    for (const key of Object.keys(json)) {
      const value = object[key];
      if (!(key in object)) found.push(path + key);
      else if (value !== null && typeof value === "object" && "objectName" in value && json[key] !== null && typeof json[key] === "object")
        found = found.concat(unknownKeys(json[key], value, path + key + "."));
    }
    return found;
  }

  FileView {
    path: (Quickshell.env("XDG_CONFIG_HOME") || Quickshell.env("HOME") + "/.config") + "/desktop-shell/config.json"

    // A misspelt setting would otherwise do nothing, silently.
    onLoaded: {
      try {
        const unknown = root.unknownKeys(JSON.parse(text()), adapter, "");
        if (unknown.length) console.warn("config.json: ignoring unknown settings:", unknown.join(", "));
      } catch (error) {
        console.warn("config.json:", error);
      }
    }

    JsonAdapter {
      id: adapter

      property JsonObject theme: JsonObject {
        property color background: "#1d1f21"
        property color foreground: "#c5c8c6"
        property color primary: "#81a2be"
        property color alert: "#cc6666"
        property color warning: "#f0c674"
        property real surfaceOpacity: 0.92
        property real highlightOpacity: 0.25
        property real dimOpacity: 0.5
        // Milliseconds a fade or colour change takes; 0 for none.
        property int animationDuration: 100
        property string monospaceFont: "monospace"
        property int fontSize: 11
        property int radius: 10
        property int itemRadius: 6
        property int iconSize: 24
        property int borderWidth: 2
        property int spacing: 8
        property int padding: 14
        property int itemPaddingX: 8
        property int itemPaddingY: 5
        property int listSpacing: 0
        property int edgeMargin: 35
        property int osdLength: 160
        property int osdIconSize: 28
        property int meterThickness: 6
        property int meterLength: 48
      }

      property JsonObject menu: JsonObject {
        property var entries: []
        // The top level's header, like waybar's modules-left, -center and -right: built-in items and integrations, by name (README.md#menu).
        property JsonObject header: JsonObject {
          property var left: ["clock"]
          property var center: []
          property var right: ["notifications", "keepAwake", "keyboardLayout", "battery"]
        }
        property int width: 960
        property int columns: 2
      }

      property JsonObject clock: JsonObject {
        // strftime, as waybar's clock takes it (see services/Clock.qml); unset, the locale's long date and short time.
        property string format
        // The locale the clock and calendar name days and months in (en_GB); unset, the system's for dates (LC_TIME).
        property string locale
        // The calendar's, 0 for Sunday to 6 for Saturday; unset (-1), the locale's.
        property int firstDayOfWeek: -1
      }

      property JsonObject calendar: JsonObject {
        // The events file the month shows (README.md#menu); unset or not there yet, the month alone.
        property string file
        // How many days from today the agenda lists, and its height in two-line events.
        property int agendaDays: 7
        property int agendaRows: 6
      }

      property JsonObject popups: JsonObject {
        // How long a pop-up stays, in milliseconds.
        property int timeout: 1500
      }

      property JsonObject audio: JsonObject {
        // Prints one device per line: kind (sink or source), name, type and friendly name, tab separated, internal first.
        property string devices
        // Given a kind and a device name, makes it the default.
        property string select
        // The pop-up wheel's commands, down then up, for the sink and the source.
        property var adjust: ({})
        // By device type (headphones, headset, internal, external, microphone), and microphoneOff.
        property var glyphs: ({})
      }

      property JsonObject brightness: JsonObject {
        // Prints the path of a file holding the backlight's value, a tab, and its maximum.
        property string backlight
        // The pop-up wheel's commands, down then up.
        property var adjust: []
      }

      property JsonObject display: JsonObject {
        // Prints the display layout, then the layouts the connected screens allow, one per line.
        property string status
        // What the Display row calls each layout.
        property var layouts: ({})
        // The laptop's panel, the screen the backlight dims.
        property string panel
        // By layout.
        property var glyphs: ({})
      }

      property JsonObject notifications: JsonObject {
        // Prints true or false for do-not-disturb, then again on every change.
        property string watch
        // Pauses notifications, or resumes them.
        property string toggle
        // Its compositor key, shown in the header while Super is held.
        property string shortcut
      }

      property JsonObject keepAwake: JsonObject {
        // Inhibits idle until its stdin closes.
        property string command
        property string shortcut
      }

      property JsonObject keyboardLayout: JsonObject {
        // A short label per layout, in the compositor's order.
        property var labels: []
        property string shortcut
      }

      property JsonObject battery: JsonObject {
        property int low: 20
        property int critical: 5
        // One per 10%, empty to full.
        property var glyphs: ({ discharging: [], charging: [] })
      }

      property JsonObject music: JsonObject {
        // The MPRIS identity of the player the Music status follows; unset, the one playing, else any.
        property string player
        // music, shuffle and repeat.
        property var glyphs: ({})
      }

      // Optional parts by name, each { source, settings }: see README.md#integrations.
      property var integrations: ({})
    }
  }
}
