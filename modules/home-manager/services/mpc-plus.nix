{ lib, pkgs, config, ... }:
let
  cfg = config.custom.services.mpc-plus;
in
{
  options.custom.services.mpc-plus = {
    enable = lib.mkEnableOption "mpc-plus";
    package = lib.mkOption {
      type = lib.types.package;
      default = config.custom.programs.mpc-plus.package;
    };
  };
  config = lib.mkIf cfg.enable {
    assertions = [ (lib.hm.assertions.assertPlatform "custom.services.mpc-plus" pkgs lib.platforms.linux) ];
    systemd.user.services.mpc-plus = {
      Install.WantedBy = [ "graphical-session.target" ];
      Unit = {
        Description = "mpc-plus notifications daemon";
        After = [ "mpd.service" ];
        PartOf = [ "mpd.service" ];
      };
      Service = {
        Type = "simple";
        Restart = "on-failure";
        RestartSec = "5s";
        ExecStart = ''${lib.getExe cfg.package} notifications-daemon'';
      };
    };
  };
}
