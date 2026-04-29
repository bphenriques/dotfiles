{ lib, pkgs, ... }:
lib.mkIf pkgs.stdenv.isLinux {
  home.packages = [ pkgs.discord ];
  xdg.mimeApps.defaultApplications."x-scheme-handler/discord" = [ "discord.desktop" ];

  custom.programs.niri.windowRules.byApp = [
    ''
      window-rule {
        match app-id="discord"
        default-column-width { proportion 0.33333; }
      }
    ''
  ];
}