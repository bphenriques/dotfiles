import { BatteryLabel } from './battery.js'
import { Clock } from './clock.js'
import { ClientTitle } from './hyprland-active.js' /* FIXME: Use plain titles per window */
import { Volume } from './volume.js'
import { Media } from './media.js'
import { SysTray } from './systray.js'
import { Launcher } from './launcher.js'
import { Workspaces } from './hyprland-workspaces.js'

export function Bar(monitor: number){
  return Widget.Window({
      name: `bar-${monitor}`,
      class_name: "bar",
      anchor: ["top", "left", "right"],
      exclusivity: "exclusive",
      child: Widget.CenterBox({
          start_widget: Widget.Box({
            spacing: 8,
            children: [
              Launcher(),
            ]
          }),
          center_widget: Widget.Box({ spacing: 8, children: [ Media() ] }),
          end_widget: Widget.Box({ hpack: "end", spacing: 8, children: [
            //Brightness(),
            Volume(),
            BatteryLabel(),
            Clock(),
            SysTray()
          ] }),
      }),
  })
}

// FIXME monitors: https://github.com/kotontrion/dotfiles/blob/main/.config%2Fags%2Fconfig.js#L60-L74
// FIXME: https://github.com/end-4/dots-hyprland
// FIXME: https://github.com/kotontrion/dotfiles