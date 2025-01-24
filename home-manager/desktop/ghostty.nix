{ config, pkgs, lib, self, ... }:

let
  theme = self.themes.doom-one;
  font = theme.font.monospace;
in
{
  programs.ghostty = {
    enable = true;
    package = if pkgs.stdenv.isLinux then pkgs.ghostty else null;
    enableFishIntegration = config.programs.fish.enable;
    themes = {
      doom-one = {
        palette = [
          "0=${theme.color0}"
          "1=${theme.color1}"
          "2=${theme.color2}"
          "3=${theme.color3}"
          "4=${theme.color4}"
          "5=${theme.color5}"
          "6=${theme.color6}"
          "7=${theme.color7}"
          "8=${theme.color8}"
          "9=${theme.color9}"
          "10=${theme.color10}"
          "11=${theme.color11}"
          "12=${theme.color12}"
          "13=${theme.color13}"
          "14=${theme.color14}"
          "15=${theme.color15}"
        ];
        background = theme.background;
        foreground = theme.foreground;
        cursor-color = theme.cursor;
        cursor-text = theme.cursorText;
        selection-background = theme.selectionForeground;
        selection-foreground = theme.selectionBackground;
      };
    };

    settings = let
      common = {
        font-family = font.name;
        font-size = font.size;
        copy-on-select = "clipboard";
        theme = "doom-one";
      };

      linux = lib.optionalAttrs pkgs.stdenv.isLinux {
        gtk-single-instance = true;
        window-decoration = true;
      };

      darwin = lib.optionalAttrs pkgs.stdenv.isDarwin {
        window-colorspace = "display-p3";
        macos-non-native-fullscreen = "visible-menu";
        macos-option-as-alt = "left";
        mouse-hide-while-typing = true;
      };

    in common // linux // darwin;
  };

  xdg.mimeApps.defaultApplications = {
    "x-scheme-handler/terminal" = [ "Ghostty.desktop" ];
    "x-scheme-handler/x-executable" = [ "Ghostty.desktop" ];
  };
}


