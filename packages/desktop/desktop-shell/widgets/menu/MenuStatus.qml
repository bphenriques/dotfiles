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
  property string icon: ""
  property bool quiet: false
  property bool active: false
  property var pins: []
  // Only the glyph and detail, e.g. on or off, without the text.
  property bool compact: false

  readonly property var value: valueFor(status)

  glyph: status ? glyphFor(status) : icon
  // In the header an icon sits closer to its own text than to its neighbours.
  spacing: quiet ? Config.theme.spacing / 2 : Config.theme.spacing
  text: compact ? "" : value.text || fallback
  detail: value.detail || fallbackDetail
  // Volumes run from "5%" to "muted": room for the longest, so the glyph beside it holds still.
  detailChars: compact && (status === "sink" || status === "source") ? 5 : 0
  // Dim when quiet (the header) or when a status has nothing to report, e.g. music while mpd is not running.
  tone: active ? "primary"
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
    case "brightness": return Glyphs.brightness(Brightness.percentage);
    case "bluetooth": return !Bluez.enabled ? Glyphs.bluetoothOff : Bluez.connected.length ? Glyphs.bluetoothConnected : Glyphs.bluetooth;
    case "display": return Glyphs.display(Display.layout);
    case "tray": return Glyphs.tray;
    case "wallpaper": return Glyphs.wallpaper;
    case "battery": return Power.present ? Glyphs.battery(Power.percentage, Power.pluggedIn) : "";
    case "layout": return Glyphs.keyboard;
    default: return "";
    }
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
      const modes = [Music.repeat ? Glyphs.repeat : "", Music.shuffle ? Glyphs.shuffle : "", Music.playing ? "" : "paused"];
      return { text: Music.title + (Music.artist ? " · " + Music.artist : ""), detail: modes.filter(mode => mode).join(" ") };
    }
    case "network":
      return { text: Network.kind === "wifi" ? pinned(Network.ssid, Network.ssid) : Network.kind === "wired" ? "Ethernet" : "Disconnected", detail: Network.wifiEnabled ? "" : "Wi-Fi off" };
    case "brightness":
      return { text: "", detail: Brightness.lit ? Brightness.percentage + "%" : "" };
    case "bluetooth": {
      if (!Bluez.present) return { text: "", detail: "" };
      if (!Bluez.enabled) return { text: "", detail: "off" };
      const device = Bluez.connected[0];
      if (!device) return { text: "", detail: "on" };
      return { text: Bluez.connected.map(device => pinned(device.address, device.name)).join(", "), detail: device.batteryAvailable ? Math.round(device.battery * 100) + "%" : "" };
    }
    case "display":
      return { text: Config.display.layouts[Display.layout] ?? "", detail: Brightness.lit ? Brightness.percentage + "%" : "" };
    case "tray":
      return { text: Tray.items.map(app => pinned(app.id, app.title || app.id)).join(", "), detail: "" };
    case "wallpaper":
      return { text: Wallpaper.name, detail: "" };
    case "battery":
      return { text: Power.present ? Power.percentage + "%" : "", detail: "" };
    case "layout":
      return { text: KeyboardLayout.label || KeyboardLayout.name, detail: "" };
    default:
      return { text: "", detail: "" };
    }
  }
}
