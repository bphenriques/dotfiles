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
    };

    # TODO: Shortcut to see current battery, current music, current output
    menus.global = [
      { key = "n"; desc = "Network"; cmd = "${terminal} --title=nmtui-tui ${lib.getExe' pkgs.networkmanager "nmtui"}"; }
      { key = "d"; desc = "Display"; cmd = (lib.getExe pkgs.wdisplays); }
    ] ++ lib.optionals config.custom.programs.session.enable [
      { key = "q"; desc = "Session"; cmd = config.custom.programs.session.exec.menu; }
    ] ++ lib.optionals config.custom.programs.volume-osd.enable [
      { key = "a"; desc = "Audio"; submenu = config.custom.programs.wlr-which-key.menus.volume-osd; }
    ] ++ lib.optionals config.custom.programs.mpc-util.enable [
      { key = "m"; desc = "Music"; submenu = config.custom.programs.wlr-which-key.menus.mpc; }
    ] ++ lib.optionals config.custom.programs.screen-recorder.enable [
      { key = "r"; desc = "Record Screen"; submenu = config.custom.programs.wlr-which-key.menus.screen-recorder; }
    ] ++ lib.optionals config.custom.programs.screenshot.enable [
      { key = "s"; desc = "Screenshot"; submenu = config.custom.programs.wlr-which-key.menus.screenshot; }
    ];
  };
}
