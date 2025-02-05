{ config, pkgs, lib, ... }:
let
  theme = config.custom.theme;
  hexToRGB = hex: lib.strings.removePrefix "#" hex;
in
lib.mkIf pkgs.stdenv.isLinux {
  programs.foot = {
    enable = true;
    server.enable = true;
    settings = {
      main.font = "${theme.fonts.monospace.name}:size=${toString theme.terminal.font-size}";
      scrollback.lines = 10000;
      mouse.hide-when-typing = "yes";

      csd.hide-when-maximized = true;

      colors = {
        foreground = hexToRGB theme.terminal.foreground;
        background = hexToRGB theme.terminal.background;
        selection-foreground = hexToRGB theme.terminal.selection-foreground;
        selection-background = hexToRGB theme.terminal.selection-background;

        regular0 = hexToRGB theme.palette.base0;
        regular1 = hexToRGB theme.palette.base1;
        regular2 = hexToRGB theme.palette.base2;
        regular3 = hexToRGB theme.palette.base3;
        regular4 = hexToRGB theme.palette.base4;
        regular5 = hexToRGB theme.palette.base5;
        regular6 = hexToRGB theme.palette.base6;
        regular7 = hexToRGB theme.palette.base7;

        bright0 = hexToRGB theme.palette.base8;
        bright1 = hexToRGB theme.palette.base9;
        bright2 = hexToRGB theme.palette.base10;
        bright3 = hexToRGB theme.palette.base11;
        bright4 = hexToRGB theme.palette.base12;
        bright5 = hexToRGB theme.palette.base13;
        bright6 = hexToRGB theme.palette.base14;
        bright7 = hexToRGB theme.palette.base15;
      };
    };
  };
}


