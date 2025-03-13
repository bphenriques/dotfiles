{ config, pkgs, lib, ... }:
let
  inherit (config.lib.stylix) colors;
  inherit (config.stylix) fonts;

  terminal              = lib.getExe' pkgs.foot "footclient";
  network-manager       = "${terminal} --title=nmtui-tui ${lib.getExe' pkgs.networkmanager "nmtui"}";

  cmd = desc: cmd: { inherit desc cmd; };
  submenu = desc: submenu: { inherit desc submenu; };
in
lib.mkIf pkgs.stdenv.isLinux {
  custom.programs.wlr-which-key = {
    enable = true;
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

    menus.global = {
      n = cmd "Network" network-manager;
      o = cmd "Output" (lib.getExe pkgs.wdisplays);
      q = lib.mkIf config.custom.programs.session.enable
        (cmd "Session" config.custom.programs.session.exec.menu);
      a = lib.mkIf config.custom.programs.volume-osd.enable
        (submenu "Audio" config.custom.programs.wlr-which-key.menus.volume-osd);
      r = lib.mkIf config.custom.programs.screen-recorder.enable
        (submenu "Record Screen" config.custom.programs.wlr-which-key.menus.screen-recorder);
      s = lib.mkIf config.custom.programs.screenshot.enable
        (submenu "Screenshot" config.custom.programs.wlr-which-key.menus.screenshot);
    };
  };
}
