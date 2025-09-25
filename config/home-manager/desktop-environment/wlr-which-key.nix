{ config, pkgs, lib, ... }:
let
  inherit (config.lib.stylix) colors;
  inherit (config.stylix) fonts;

  terminal = lib.getExe' pkgs.foot "footclient";
in
lib.mkIf pkgs.stdenv.isLinux {
  custom.programs.wlr-which-key = {
    enable = true;
    package = pkgs.wlr-which-key-git;
    settings = {
      font = "${fonts.monospace.name} ${toString fonts.sizes.popups}";
      background = colors.withHashtag.base00 + "dd";
      color = colors.withHashtag.base06;
      border = colors.withHashtag.base0D;
      border_width = 1;
      corner_r = 10;
      separator = " ➜ ";

      anchor = "bottom";
      margin_right = 0;
      margin_bottom = 30;
      margin_left = 0;
      margin_top = 0;

      rows_per_column = 5;
      column_padding = 50;
    };

    menus.global = lib.optionals config.custom.programs.mpc-plus.enable [
      { key = "m"; desc = "Music"; submenu = config.custom.programs.wlr-which-key.menus.mpc-plus; }
    ] ++ lib.optionals config.custom.programs.volume-osd.enable [
      { key = "a"; desc = "Audio Output"; submenu = config.custom.programs.wlr-which-key.menus.sound-output; }
      { key = "A"; desc = "Audio Input"; submenu = config.custom.programs.wlr-which-key.menus.sound-input; }
    ] ++ [
      { key = "d"; desc = "Display Layout"; cmd = (lib.getExe pkgs.wdisplays); }
      { key = "n"; desc = "Network Manager"; cmd = "${terminal} --title=nmtui-tui ${lib.getExe' pkgs.networkmanager "nmtui"}"; }
    ] ++ lib.optionals config.custom.programs.session.enable [
      { key = "q"; desc = "Session"; cmd = config.custom.programs.session.exec.menu; }
    ];
  };
}
