{ config, pkgs, lib, self, ... }:
let
  theme = config.custom.theme;
in
{
  programs.ghostty = {
    enable = true;
    package = if pkgs.stdenv.isLinux then pkgs.ghostty else null;
    enableFishIntegration = config.programs.fish.enable;
    themes = {
      custom = {
        inherit (theme.terminal) background foreground cursor-color cursor-text selection-background selection-foreground;
        palette = [
          "0=${theme.palette.base0}"
          "1=${theme.palette.base1}"
          "2=${theme.palette.base2}"
          "3=${theme.palette.base3}"
          "4=${theme.palette.base4}"
          "5=${theme.palette.base5}"
          "6=${theme.palette.base6}"
          "7=${theme.palette.base7}"
          "8=${theme.palette.base8}"
          "9=${theme.palette.base9}"
          "10=${theme.palette.base10}"
          "11=${theme.palette.base11}"
          "12=${theme.palette.base12}"
          "13=${theme.palette.base13}"
          "14=${theme.palette.base14}"
          "15=${theme.palette.base15}"
        ];
      };
    };

    settings = let
      common = {
        font-family = theme.fonts.monospace.name;
        font-size = theme.terminal.font-size;
        theme = "custom";
        copy-on-select = "clipboard";
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
    in lib.mergeAttrsList [ common linux darwin ];
  };

  xdg.mimeApps.defaultApplications = {
    "x-scheme-handler/terminal" = [ "Ghostty.desktop" ];
    "x-scheme-handler/x-executable" = [ "Ghostty.desktop" ];
  };
}


