{ pkgs, lib, config, self, ... }:
let
  terminal = config.custom.programs.terminal;
  system-monitor = terminal.execApp { title = "btop-tui"; cmd = lib.getExe config.programs.btop.package; };
in
{
  programs.btop = {
    enable = true;
    settings = {
      theme_background = true;
      proc_gradient = false;
      graph_symbol = "block";
      shown_boxes = "cpu mem proc";
    };
  };
  stylix.targets.btop.enable = true;

  home.packages = lib.optionals pkgs.stdenv.isLinux [
    (pkgs.makeDesktopItem {
      name = "system-monitor";
      desktopName = "System Monitor";
      icon = self.lib.builders.mkNerdFontIcon { textColor = config.lib.stylix.colors.withHashtag.base07; } "system-monitor" "";
      exec = system-monitor;
    })
  ];

  custom.programs.niri.bindings = lib.optionalAttrs pkgs.stdenv.isLinux {
    "Ctrl+Shift+Escape" = ''spawn-sh "${system-monitor}"'';
  };
}
