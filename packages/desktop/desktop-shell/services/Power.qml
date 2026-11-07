pragma Singleton

import QtQuick
import Quickshell
import Quickshell.Services.UPower
import qs

// Plugged in is read from the battery's own state, the same source as its percentage, not UPower's separate onBattery flag.
Singleton {
  readonly property var battery: UPower.displayDevice
  readonly property bool present: battery.isLaptopBattery
  readonly property int percentage: Math.round(battery.percentage * 100)
  readonly property bool pluggedIn: [UPowerDeviceState.Charging, UPowerDeviceState.FullyCharged, UPowerDeviceState.PendingCharge].includes(battery.state)
  // Warning levels apply only on battery, at the upower-notify thresholds.
  readonly property string level: pluggedIn ? "normal"
    : percentage <= Config.battery.critical ? "critical"
    : percentage <= Config.battery.low ? "low"
    : "normal"
}
