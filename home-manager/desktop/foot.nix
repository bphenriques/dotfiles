{ config, pkgs, lib, self, ... }:

# TODO: https://github.com/nix-community/home-manager/commit/5f6aa268e419d053c3d5025da740e390b12ac936
let
  inherit (self.themes.lib) hexToRGB;
  theme = self.themes.doom-one;
  font = theme.font.monospace;
in
lib.mkIf pkgs.stdenv.isLinux {
  programs.foot = {
    enable = true;
    server.enable = true;
    settings = {
      main = {
        font = "${font.name}:size=${toString font.size}";
      };
      scrollback = {
        lines = 10000;
      };
      mouse = {
        hide-when-typing = "yes";
      };

      colors = {
        foreground = hexToRGB theme.foreground;
        background = hexToRGB theme.background;
        regular0 = hexToRGB theme.color0;
        regular1 = hexToRGB theme.color1;
        regular2 = hexToRGB theme.color2;
        regular3 = hexToRGB theme.color3;
        regular4 = hexToRGB theme.color4;
        regular5 = hexToRGB theme.color5;
        regular6 = hexToRGB theme.color6;
        regular7 = hexToRGB theme.color7;

        bright0 = hexToRGB theme.color8;
        bright1 = hexToRGB theme.color9;
        bright2 = hexToRGB theme.color10;
        bright3 = hexToRGB theme.color11;
        bright4 = hexToRGB theme.color12;
        bright5 = hexToRGB theme.color13;
        bright6 = hexToRGB theme.color14;
        bright7 = hexToRGB theme.color15;

        selection-foreground = hexToRGB theme.selectionForeground;
        selection-background = hexToRGB theme.selectionBackground;
      };
    };
  };
}


