{ config, lib, pkgs, ... }:
{
  systemd.user.services.swww = {
    Unit = {
      Description = "Efficient animated wallpaper daemon for wayland";
      PartOf = [ "graphical-session.target" ];
      After = [ "graphical-session.target" ];
    };

    # TODO: Ensure WAYLAND_DISPLAY is set
    Install.WantedBy = [ config.wayland.systemd.target ];
    Service = {
      Type = "simple";
      ExecStart = ''${pkgs.swww}/bin/swww-daemon'';
      ExecStop = "${pkgs.swww}/bin/swww kill";
      Restart = "on-failure";
    };
  };

  home.packages = [ pkgs.swww ];
}
