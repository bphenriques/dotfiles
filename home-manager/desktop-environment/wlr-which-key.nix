{ config, pkgs, lib, ... }:
let
  inherit (config.lib.stylix) colors;
  inherit (config.stylix) fonts;

  cmd = desc: cmd: { inherit desc cmd; };
in
{
  custom.programs.wlr-which-key = {
    enable = true;
    settings = {
      font = "${fonts.monospace.name} ${toString fonts.sizes.popups}";
      background = colors.withHashtag.base00 + "dd";
      color = colors.withHashtag.base06;
      border = colors.withHashtag.base0D;
      border_width = 1;
      corner_r = 10;
      separator = "\t";

      anchor = "bottom";
      margin_right = 0;
      margin_bottom = 30;
      margin_left = 0;
      margin_top = 0;
    };
  };
}
