{ config, pkgs, lib, ... }:
{
  stylix.targets.ghostty.enable = true;
  programs.ghostty = {
    enable = true;
    enableFishIntegration = config.programs.fish.enable;
    systemd.enable = pkgs.stdenv.isLinux;

    settings = let
      common = {
        copy-on-select = "clipboard";
        command = lib.getExe config.programs.fish.package;
      };

      linux = lib.optionalAttrs pkgs.stdenv.isLinux {
        gtk-single-instance = true;
        window-decoration = true;
        quit-after-last-window-closed = false; # Reduces latency when opening new windows
      };

      darwin = lib.optionalAttrs pkgs.stdenv.isDarwin {
        window-colorspace = "display-p3";
        macos-non-native-fullscreen = "visible-menu";
        macos-option-as-alt = "left";
        mouse-hide-while-typing = true;
      };
    in lib.mergeAttrsList [ common linux darwin ];
  };

  xdg.mimeApps.defaultApplications = lib.mkIf pkgs.stdenv.isLinux {
    "x-scheme-handler/terminal" = [ "Ghostty.desktop" ];
    "x-scheme-handler/x-executable" = [ "Ghostty.desktop" ];
  };

  custom.programs.niri = lib.mkIf pkgs.stdenv.isLinux {
    spawnShAtStartup = [ "${lib.getExe pkgs.ghostty} +new-window" ];
    windowRules = {
      byApp = [
        ''
          window-rule {
            match app-id="com.mitchellh.ghostty"
            default-column-width { proportion 0.5; }
          }
        ''
      ];
      byType.tui = lib.map (title: ''title="${title}"'') [
        "btop-tui"
        "yazi-tui"
      ];
    };
  };
}


