pragma Singleton

import QtQuick
import Quickshell
import Quickshell.Networking

Singleton {
  readonly property var device: Networking.devices.values.find(device => device.connected) ?? null
  // "wifi", "wired" or "none".
  readonly property string kind: device === null ? "none" : device.type === DeviceType.Wired ? "wired" : "wifi"
  readonly property string ssid: kind === "wifi" ? (device.networks.values.find(network => network.connected)?.name ?? device.name) : ""
}
