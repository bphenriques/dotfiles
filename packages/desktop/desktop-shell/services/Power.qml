pragma Singleton

import QtQuick
import Quickshell
import Quickshell.Services.UPower
import qs

// Plugged in is read from the battery's own state, the same source as its percentage, not UPower's separate onBattery flag.
Singleton {
  // Nothing until UPower has answered: before that the percentage reads 0, which would show as critical.
  readonly property bool present: UPower.displayDevice.ready && UPower.displayDevice.isLaptopBattery
  readonly property int percentage: Math.round(UPower.displayDevice.percentage * 100)
  readonly property bool pluggedIn: [UPowerDeviceState.Charging, UPowerDeviceState.FullyCharged, UPowerDeviceState.PendingCharge].includes(UPower.displayDevice.state)
  // Warning levels apply only on battery, at the upower-notify thresholds.
  readonly property string level: !present || pluggedIn ? "normal"
    : percentage <= Config.battery.critical ? "critical"
    : percentage <= Config.battery.low ? "low"
    : "normal"
}
