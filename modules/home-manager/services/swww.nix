{ lib, pkgs, config, self, ... }:
let
  cfg = config.custom.services.swww;
in
{
  options.custom.services.swww = {
    directory = lib.mkOption {
      description = "Location of wallpaper";
      type = lib.types.str;
    };
  };

  config = {
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
    };
  };
}
