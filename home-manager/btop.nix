{ pkgs, lib, config, self, ... }:
{
  programs.btop = {
    enable = true;
    settings = {
      theme_background = true;
      proc_gradient = false;
      graph_symbol = "block";
      shown_boxes = "cpu mem proc";
      clock_format = "";
    };
  };
  stylix.targets.btop.enable = true;

  home.packages = lib.optionals pkgs.stdenv.isLinux [
    (pkgs.makeDesktopItem {
      name = "system-monitor";
      desktopName = "System Monitor";
      icon = self.lib.builders.mkNerdFontIcon pkgs { textColor = config.lib.stylix.colors.withHashtag.base07; } "system-monitor" "";
      exec = ''${lib.getExe' pkgs.foot "footclient"} --title=btop-tui ${lib.getExe pkgs.btop}'';
    })
  ];
}
