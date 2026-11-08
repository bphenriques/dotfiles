{ lib, config, self, pkgs, ... }:
let
  wallpapers = "${self.packages.wallpapers}/share/wallpapers";

  awww-util = lib.getExe self.packages.awww-util;
in
lib.mkIf pkgs.stdenv.hostPlatform.isLinux {
  services.awww.enable = true;
  home.packages = [ self.packages.awww-util ];

  # The wallpaper state file must exist up front for the desktop shell to watch it.
  systemd.user.tmpfiles.rules = [ "f %t/wallpaper 0600 - - -" ];

  my.programs.desktop-shell = {
    menu.entries = lib.mkOrder 600 [
      { key = "w"; desc = "Wallpaper"; width = 640; status = "wallpaper"; submenu = [
        { key = [ "Left" "Right" ]; desc = "Wallpaper"; width = 640; cmd = [ "${awww-util} prev ${wallpapers}" "${awww-util} next ${wallpapers}" ]; keepOpen = true; }
        { key = "r";                desc = "Random";    cmd = "${awww-util} random ${wallpapers}"; keepOpen = true; }
      ]; }
    ];
  };

  systemd.user.services.set-wallpaper = {
    Unit = {
      Description = "Sets the wallpaper";
      PartOf = [ "awww.service" ];
      After = [ "awww.service" ];
    };
    Install.WantedBy = [ config.wayland.systemd.target ];
    Service = {
      Type = "oneshot";
      # awww-daemon signals no readiness, so the first attempt can beat its socket.
      Restart = "on-failure";
      RestartSec = 1;
      ExecStart = lib.escapeShellArgs [ awww-util "random" wallpapers ];
    };
  };

  my.programs.niri.layerRules.base = [
    ''
      layer-rule {
        match namespace="^awww-daemon$"
        place-within-backdrop true
      }
    ''
  ];
}
