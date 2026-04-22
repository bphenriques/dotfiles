{ lib, pkgs, ... }:
lib.mkIf pkgs.stdenv.isLinux {
  home.packages = [ pkgs.qbittorrent ];

  custom.programs.niri.windowRules.byApp = [
    ''
      window-rule {
        match app-id="org.qbittorrent.qBittorrent"
        open-floating true
        open-focused true
        default-column-width { fixed 900; }
        default-window-height { fixed 600; }
      }
    ''
  ];
}
