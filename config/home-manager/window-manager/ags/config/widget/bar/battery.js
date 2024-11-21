const battery = await Service.import('battery')

export function BatteryLabel() {
    const icon = battery.bind("percent").as(p => `battery-level-${Math.floor(p / 10) * 10}-symbolic`)

    return Widget.Box({
        class_name: "battery",
        visible: battery.bind("available"),
        children: [
            Widget.Icon({ icon }),
        ],
    })
}