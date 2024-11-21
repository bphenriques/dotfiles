export function Launcher() {
  return Widget.Button({
      class_name: 'launcher',
      on_primary_click_release: (evt) => Utils.exec("fuzzel"),
      //child: Widget.Icon({icon: "nix-snowflake-symbolic"}),
      child: Widget.Label({label: ""}),
  })
}