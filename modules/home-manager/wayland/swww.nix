{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.custom.services.swww;
in
{
  options.custom.services.swww = {
    enable = mkEnableOption ''swww service.'';
  };

  config = mkIf cfg.enable {
    # TODO: move to a module
    systemd.user.services = {
      swww = {
        Unit = {
          Description = "Efficient animated wallpaper daemon for wayland";
          PartOf = [ "graphical-session.target" ];
          After = [ "graphical-session.target" ];
        };
        Install.WantedBy = [ "graphical-session.target" ];
        Service = {
          Type = "simple";
          ExecStart = ''${pkgs.swww}/bin/swww-daemon'';
          ExecStop = "${pkgs.swww}/bin/swww kill";
          Restart = "on-failure";
        };
      };
    };
    home.packages = [ pkgs.swww ];
  };
}
