{ config, pkgs, lib, self, ... }:
lib.mkIf pkgs.stdenv.isDarwin {
  stylix.targets.ghostty.enable = true;
  programs.ghostty = {
    enable = true;
    package = if pkgs.stdenv.isLinux then pkgs.ghostty else null;
    enableFishIntegration = config.programs.fish.enable;

    settings = let
      common = {
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


