{ lib, pkgs, ... }:
lib.mkIf pkgs.stdenv.isLinux {
  home.packages = [ pkgs.heroic ];
  xdg.mimeApps.defaultApplications."x-scheme-handler/heroic" = [ "com.heroicgameslauncher.hgl.desktop" ];

  custom.programs.niri.windowRules.byApp = [
    ''
      window-rule {
        match app-id="heroic"
        open-maximized-to-edges true
      }
    ''
  ];
}