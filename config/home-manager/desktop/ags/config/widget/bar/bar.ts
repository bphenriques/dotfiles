import { BatteryLabel } from './battery.js'
import { Clock } from './clock.js'
import { Workspaces } from './hyprland-workspaces.js'
import { ClientTitle } from './hyprland-active.js' /* FIXME: Use plain titles per window */
import { Volume } from './volume.js'
import { Media } from './media.js'
import { SysTray } from './systray.js'
import { Launcher } from './launcher.js'
``
export function Bar(monitor: number){
  return Widget.Window({
      name: `bar-${monitor}`,
      class_name: "bar",
      anchor: ["top", "left", "right"],
      exclusivity: "exclusive",
      child: Widget.CenterBox({
          start_widget: Widget.Box({ spacing: 8, children: [ Launcher(), Workspaces() ] }),
          center_widget: Widget.Box({ spacing: 8, children: [ Media() ] }),
          end_widget: Widget.Box({ hpack: "end", spacing: 8, children: [ Volume(), BatteryLabel(), Clock(), SysTray() ] }),
      }),
  })
}
