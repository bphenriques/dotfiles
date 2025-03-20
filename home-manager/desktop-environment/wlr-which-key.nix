{ config, pkgs, lib, self, ... }:
let
  inherit (config.lib.stylix) colors;
  inherit (config.stylix) fonts;

  terminal              = lib.getExe' pkgs.foot "footclient";
in
lib.mkIf pkgs.stdenv.isLinux {
  custom.programs.wlr-which-key = {
    enable = true;
    package = self.pkgs.wlr-which-key-git;
    settings = {
      font = "${fonts.monospace.name} ${toString fonts.sizes.popups}";
      background = colors.withHashtag.base00 + "dd";
      color = colors.withHashtag.base06;
      border = colors.withHashtag.base0D;
      border_width = 1;
      corner_r = 10;
      separator = "  ➜  ";

      anchor = "bottom";
      margin_right = 0;
      margin_bottom = 30;
      margin_left = 0;
      margin_top = 0;

      rows_per_column = 6;
      column_padding = 20;
    };

    menus.global = lib.optionals config.custom.programs.mpc-util.enable [
      { key = "m"; desc = "Music"; submenu = config.custom.programs.wlr-which-key.menus.mpc; }
    ] ++ lib.optionals config.custom.programs.screen-recorder.enable [
      { key = "r"; desc = "Record Screen"; submenu = config.custom.programs.wlr-which-key.menus.screen-recorder; }
    ] ++ lib.optionals config.custom.programs.screenshot.enable [
      { key = "s"; desc = "Screenshot"; submenu = config.custom.programs.wlr-which-key.menus.screenshot; }
    ] ++ lib.optionals config.custom.programs.volume-osd.enable [
      { key = "a"; desc = "Audio Output"; submenu = config.custom.programs.wlr-which-key.menus.sound-output; }
      { key = "Ctrl+a"; desc = "Audio Input"; submenu = config.custom.programs.wlr-which-key.menus.sound-input; }
    ] ++ [
      { key = "d"; desc = "Display Output"; cmd = (lib.getExe pkgs.wdisplays); }
      { key = "n"; desc = "Network Manager"; cmd = "${terminal} --title=nmtui-tui ${lib.getExe' pkgs.networkmanager "nmtui"}"; }
    ] ++ lib.optionals config.custom.programs.session.enable [
      { key = "q"; desc = "Session"; cmd = config.custom.programs.session.exec.menu; }
    ];
  };
}
