{ lib, config, self, pkgs, ... }:
let
  wallpapers = "${self.packages.wallpapers}/share/wallpapers";
in
lib.mkIf pkgs.stdenv.hostPlatform.isLinux {
  services.awww.enable = true;
  home.packages = [ self.packages.awww-util ];

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
      ExecStart = lib.escapeShellArgs [ "${lib.getExe self.packages.awww-util}" "random" wallpapers ];
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
