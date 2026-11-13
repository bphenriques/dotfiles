import QtQuick
import QtQuick.Layouts
import qs
import qs.services
import qs.ui

// Status line for a menu status id (`status` in my.programs.desktop-shell.menu.entries), or an icon and label without one.
StatusLine {
  id: root

  required property string status
  property string fallback: ""
  property string fallbackDetail: ""
  property var fallbackGlyphs: []
  property string icon: ""
  property bool quiet: false
  property bool active: false
  property var pins: []
  // Only the glyph and detail, e.g. on or off, without the text.
  property bool compact: false

  // The integration a status not built in names, if any.
  readonly property var integration: Integrations.get(status)
  readonly property var value: valueFor(status)

  glyph: status ? glyphFor(status) : icon
  // In the header an icon sits closer to its own text than to its neighbours.
  spacing: quiet ? Config.theme.spacing / 2 : Config.theme.spacing
  text: compact ? "" : value.text || fallback
  detail: value.detail || fallbackDetail
  detailGlyphs: value.glyphs ?? fallbackGlyphs
  // Volumes run from "5%" to "muted": room for the longest, so the glyph beside it holds still.
  detailChars: compact && (status === "sink" || status === "source") ? 5 : 0
  // Dim when quiet (the header) or when a status has nothing to report, e.g. music while mpd is not running.
  tone: status === "notifications" && DoNotDisturb.enabled ? "alert"
    : active || status === "keepAwake" && IdleInhibit.enabled || quiet && integration?.active ? "primary"
    : status === "battery" && Power.level === "critical" ? "alert"
    : status === "battery" && Power.level === "low" ? "warning"
    : quiet || status && !value.text && !value.detail ? "dim"
    : "normal"

  // The input moving as you speak, only in its own level: beside the volume on the top row it reads as a picture of that number.
  Meter {
    visible: root.compact && root.status === "source"
    // A neighbour, not part of the value: the full gap, half of it from the row.
    Layout.leftMargin: Config.theme.spacing / 2
    Layout.preferredWidth: Config.theme.meterLength
    value: Audio.sourceMuted ? 0 : Audio.sourceLevel
  }

  function pinned(id, name) {
    return Array.from(pins).find(pin => MenuKeys.glob(pin.match, id))?.label ?? name;
  }

  function glyphFor(status) {
    switch (status) {
    case "sink": return Audio.sinkMuted ? Glyphs.volumeOff : Glyphs.audioDevice(Audio.sinkType);
    case "source": return Audio.sourceMuted ? Glyphs.microphoneOff : Glyphs.audioDevice(Audio.sourceType);
    case "music": return Glyphs.music;
    case "network": return Network.kind === "wifi" ? Glyphs.wifi : Network.kind === "wired" ? Glyphs.ethernet : Glyphs.disconnected;
    case "bluetooth": return !Bluez.enabled ? Glyphs.bluetoothOff : Bluez.connected.length ? Glyphs.bluetoothConnected : Glyphs.bluetooth;
    case "display": return Glyphs.display(Display.layout);
    case "tray": return Glyphs.tray;
    case "calendar": return Glyphs.calendar;
    case "battery": return Power.present ? Glyphs.battery(Power.percentage, Power.pluggedIn) : "";
    case "keyboardLayout": return Glyphs.keyboard;
    case "notifications": return DoNotDisturb.enabled ? Glyphs.doNotDisturb : Glyphs.notifications;
    case "keepAwake": return Glyphs.keepAwake;
    default: return integration?.glyph ?? "";
    }
  }


  // In a level's header how old the events are; on a row the next event, and when.
  function calendarValue() {
    if (!Calendar.enabled) return { text: "", detail: "" };
    if (quiet) {
      const updated = Calendar.updated;
      return {
        text: "",
        // Its age even after a failed sync, the line under the legend saying why: the events shown are that old.
        detail: Calendar.syncing ? "syncing…" : !updated ? ""
          : "updated " + Clock.format(updated, updated.toDateString() === Clock.date.toDateString() ? "%H:%M" : "%-d %b %H:%M")
      };
    }
    const next = Calendar.next;
    if (!next) return { text: "", detail: "" };
    const today = next.start.toDateString() === Clock.date.toDateString() || next.start < Clock.date;
    const when = next.allDay ? (today ? "today" : Clock.format(next.start, "%a")) : Clock.format(next.start, today ? "%H:%M" : "%a %H:%M");
    return { text: next.title, detail: when };
  }

  function valueFor(status) {
    switch (status) {
    case "sink":
      if (!Audio.sink) return { text: "", detail: "" };
      return { text: pinned(Audio.sink.name, Audio.sinkName), detail: Audio.sinkMuted ? "muted" : Math.round(Audio.sinkVolume * 100) + "%" };
    case "source":
      if (!Audio.source) return { text: "", detail: "" };
      return { text: pinned(Audio.source.name, Audio.sourceName), detail: Audio.sourceMuted ? "muted" : Math.round(Audio.sourceVolume * 100) + "%" };
    case "music": {
      if (!Music.present || !Music.title) return { text: "", detail: "" };
      return {
        text: Music.title + (Music.artist ? " · " + Music.artist : ""),
        detail: Music.playing ? "" : "paused",
        glyphs: [Music.repeat ? Glyphs.repeat : "", Music.shuffle ? Glyphs.shuffle : ""].filter(glyph => glyph)
      };
    }
    case "network":
      return { text: Network.kind === "wifi" ? pinned(Network.ssid, Network.ssid) : Network.kind === "wired" ? "Ethernet" : "Disconnected", detail: Network.wifiEnabled ? "" : "Wi-Fi off" };
    case "bluetooth": {
      if (!Bluez.present) return { text: "", detail: "" };
      if (!Bluez.enabled) return { text: "", detail: "off" };
      const [device, ...others] = Bluez.connected;
      if (!device) return { text: "No device connected", detail: "" };
      if (others.length) return { text: Bluez.connected.length + " devices connected", detail: "" };
      return { text: pinned(device.address, device.name), detail: device.batteryAvailable ? Math.round(device.battery * 100) + "%" : "" };
    }
    case "display":
      return { text: Config.display.layouts[Display.layout] ?? "", detail: Brightness.lit ? Brightness.percentage + "%" : "" };
    case "calendar":
      return calendarValue();
    case "tray":
      return { text: Tray.items.map(app => pinned(app.id, app.title || app.id)).join(", "), detail: "" };
    case "battery":
      return { text: Power.present ? Power.percentage + "%" : "", detail: "" };
    case "keyboardLayout":
      return { text: KeyboardLayout.label || KeyboardLayout.name, detail: "" };
    default:
      return { text: integration?.text ?? "", detail: integration?.detail ?? "" };
    }
  }
}
