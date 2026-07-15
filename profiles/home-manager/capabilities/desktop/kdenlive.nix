{ lib, pkgs, ... }:
lib.mkIf pkgs.stdenv.isLinux {
  home.packages = [ pkgs.kdePackages.kdenlive ];

  custom.programs.niri.windowRules.byApp = [
    ''
      window-rule {
        match app-id="org.kde.kdenlive"
        open-maximized true
      }
    ''
  ];
}
