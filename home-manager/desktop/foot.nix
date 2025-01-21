{ config, pkgs, lib, community, ... }:

# TODO: https://github.com/nix-community/home-manager/commit/5f6aa268e419d053c3d5025da740e390b12ac936
let
  font = {
    name = "Hack Nerd Font Mono";
    size = 15;
  };

  hexToRGB = hex: lib.strings.removePrefix "#" hex;

  colors = {
    foreground            = "#bbc2cf";
    background            = "#282c34";
    selectionForeground   = "#bbc2cf";
    selectionBackground   = "#3f444a";

    cursor           = "#bbc2cf";
    cursorText       = "#282c34";

    # Black
    color0 = "#282c34";
    color8 = "#3f444a";

    # Red
    color1 = "#ff6c6b";
    color9 = "#ff6655";#

    # Green
    color2  = "#98be65";
    color10 = "#99bb66";

    # Yellow
    color3  = "#ECBE7B";
    color11 = "#ECBE7B";

    # Blue
    color4  = "#51afef";
    color12 = "#51afef";

    # Magenta
    color5  = "#c678dd";
    color13 = "#c678dd";

    # Cyan
    color6  = "#46D9FF";
    color14 = "#46D9FF";

    # White
    color7  = "#dfdfdf";
    color15 = "#bbc2cf";
  };
in
{
  programs.foot = {
    enable = true;
    server.enable = true;
    settings = {
      main = {
        shell = "fish";
        font = "${font.name}:size=${toString font.size}";
      };
      scrollback = {
        lines = 10000;
      };
      mouse = {
        hide-when-typing = "yes";
      };

      colors = {
        foreground = hexToRGB colors.foreground;
        background = hexToRGB colors.background;
        regular0 = hexToRGB colors.color0;
        regular1 = hexToRGB colors.color1;
        regular2 = hexToRGB colors.color2;
        regular3 = hexToRGB colors.color3;
        regular4 = hexToRGB colors.color4;
        regular5 = hexToRGB colors.color5;
        regular6 = hexToRGB colors.color6;
        regular7 = hexToRGB colors.color7;

        bright0 = hexToRGB colors.color8;
        bright1 = hexToRGB colors.color9;
        bright2 = hexToRGB colors.color10;
        bright3 = hexToRGB colors.color11;
        bright4 = hexToRGB colors.color12;
        bright5 = hexToRGB colors.color13;
        bright6 = hexToRGB colors.color14;
        bright7 = hexToRGB colors.color15;

        selection-foreground = hexToRGB colors.selectionForeground;
        selection-background = hexToRGB colors.selectionBackground;
      };
    };
  };
}


