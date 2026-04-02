{ lib, config, self, ... }:
let
  wallpapers = "${self.pkgs.wallpapers}/share/wallpapers";
in
{
  home.packages = [ self.pkgs.awww-util ];
  systemd.user.services.set-wallpaper = {
    Unit = {
      Description = "Sets the wallpaper";
      PartOf = [ "awww.service" ];
      After = [ "awww.service" ];
    };
    Install.WantedBy = [ config.wayland.systemd.target ];
    Service = {
      Type = "oneshot";
      ExecStart = lib.escapeShellArgs [ "${lib.getExe self.pkgs.awww-util}" "random" wallpapers ];
    };
  };
}
