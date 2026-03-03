{ lib, config, self, ... }:
let
  wallpapers = "${self.pkgs.wallpapers}/share/wallpapers";
in
{
  home.packages = [ self.pkgs.swww-util ];
  systemd.user.services.set-wallpaper = {
    Unit = {
      Description = "Sets the wallpaper";
      PartOf = [ "swww.service" ];
      After = [ "swww.service" ];
    };
    Install.WantedBy = [ config.wayland.systemd.target ];
    Service = {
      Type = "oneshot";
      ExecStart = lib.escapeShellArgs [ "${lib.getExe self.pkgs.swww-util}" "random" wallpapers ];
    };
  };
}
