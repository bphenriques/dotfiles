import { sh, range } from "lib/utils"

const hyprland = await Service.import("hyprland")
const dispatch = ws => hyprland.messageAsync(`dispatch workspace ${ws}`);

const CreateWorkspaces = (ws: number) => Widget.Box({
    children: Array.from({ length: ws }, (_, i) => i + 1).map(i => {
        const btn = Widget.Button({
          attribute: i,
          child: Widget.Label({label: `${i}`}),
          on_clicked: () => dispatch(i),
          setup: self => self.hook(hyprland, () => {
            self.toggleClassName("active", hyprland.active.workspace.id === i)
          }),
        })
        const workspace = hyprland.getWorkspace(i)
        if (workspace && workspace.initialized) {
            btn.setup()
        }
        return btn
    }),
    setup: box => {
        //box.hook(hyprland.active.workspace, () => box.children.map(btn => {
        //    btn.visible = hyprland.workspaces.some(ws => ws.id === btn.attribute)
        //}))
    },
})

export function Workspaces() {
  return Widget.EventBox({
      class_name: "workspaces",
      on_scroll_up: () => dispatch("+1"),
      on_scroll_down: () => dispatch("-1"),
      child: CreateWorkspaces(10),
  })
}