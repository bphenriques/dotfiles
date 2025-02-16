{ config, lib, pkgs, self, ... }:
{
  home.packages = [ pkgs.swww ];

  systemd.user.services = {
    swww = {
      Unit = {
        Description = "Efficient animated wallpaper daemon for wayland";
        ConditionEnvironment = [ "WAYLAND_DISPLAY" ];
        PartOf = [ "graphical-session.target" ];
        After = [ "graphical-session.target" ];
      };
      Install.WantedBy = [ config.wayland.systemd.target ];
      Service = {
        Type = "simple";
        ExecStart = lib.getExe' pkgs.swww "swww-daemon";
        ExecStop = "${lib.getExe pkgs.swww} kill";
        Restart = "on-failure";
      };
    };

    set-wallpaper = {
      Unit = {
        Description = "Sets the wallpaper";
        PartOf = [ "swww.service" ];
        After = [ "swww.service" ];
      };
      Install.WantedBy = [ config.wayland.systemd.target ];
      Service = {
        Type = "oneshot";
        ExecStart = lib.escapeShellArgs [
          "${lib.getExe self.pkgs.swww-util}"
          "random" "${self.pkgs.wallpapers}/share/wallpapers"
        ];
      };
    };
  };
}