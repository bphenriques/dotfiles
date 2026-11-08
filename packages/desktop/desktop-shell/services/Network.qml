pragma Singleton

import QtQuick
import Quickshell
import Quickshell.Networking

Singleton {
  readonly property var device: Networking.devices.values.find(device => device.connected) ?? null
  // "wifi", "wired" or "none".
  readonly property string kind: device === null ? "none" : device.type === DeviceType.Wired ? "wired" : "wifi"
  readonly property string ssid: kind === "wifi" ? (device.networks.values.find(network => network.connected)?.name ?? device.name) : ""

  readonly property bool wifiEnabled: Networking.wifiEnabled
  readonly property var wifi: Networking.devices.values.find(device => device.type === DeviceType.Wifi) ?? null
  // Saved networks in range, by name so their keys hold still as the signal changes.
  readonly property var known: wifiEnabled && wifi ? Array.from(wifi.networks.values).filter(network => network.known).sort((a, b) => a.name.localeCompare(b.name)) : []
}
