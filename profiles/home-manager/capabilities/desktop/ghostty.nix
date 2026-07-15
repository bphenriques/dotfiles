{ config, pkgs, lib, ... }:
let
  ghostty = lib.getExe pkgs.ghostty;
in
{
  custom.programs.terminal = {
    package = pkgs.ghostty;
    exec = "${ghostty} +new-window";
    execApp = { cmd, title ? null }: "${ghostty} +new-window${lib.optionalString (title != null) " --title=${title}"} -e ${cmd}";
  };

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
        background-opacity = 0.92; # Needed for niri blur to show through
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
    spawnShAtStartup = [ config.custom.programs.terminal.exec ];
    windowRules = {
      byApp = [
        ''
          window-rule {
            match app-id="com.mitchellh.ghostty"
            default-column-width { proportion 0.5; }
            background-effect {
              blur true
            }
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
