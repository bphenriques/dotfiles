pragma Singleton

import QtQuick
import Quickshell
import Quickshell.Networking

// The connection in use, and the Wi-Fi radio with its saved networks in range.
Singleton {
  // "wifi", "wired" or "none".
  readonly property string kind: internal.device === null ? "none" : internal.device.type === DeviceType.Wired ? "wired" : "wifi"
  readonly property string ssid: kind === "wifi" ? (internal.device.networks.values.find(network => network.connected)?.name ?? internal.device.name) : ""
  readonly property bool wifiEnabled: Networking.wifiEnabled
  // Saved networks in range, by name so their keys hold still as the signal changes.
  readonly property var known: wifiEnabled && internal.wifi ? Array.from(internal.wifi.networks.values).filter(network => network.known).sort((a, b) => a.name.localeCompare(b.name)) : []

  QtObject {
    id: internal
    readonly property var device: Networking.devices.values.find(device => device.connected && device.type !== DeviceType.None) ?? null
    readonly property var wifi: Networking.devices.values.find(device => device.type === DeviceType.Wifi) ?? null
  }
}
