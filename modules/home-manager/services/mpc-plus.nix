{ lib, pkgs, config, ... }:
let
  cfg = config.custom.services.mpc-plus;
  programCfg = config.custom.programs.mpc-plus;

  devicesWithNotifications = lib.filterAttrs (_: device: device.notifications) programCfg.devices;

  isLocalMpd = device:
    config.services.mpd.enable
    && device.host == config.services.mpd.network.listenAddress
    && device.port == config.services.mpd.network.port;

  mkService = name: device: {
    Install.WantedBy = [ "graphical-session.target" ];
    Unit = {
      Description = "mpc-plus notifications for ${name}";
    } // lib.optionalAttrs (isLocalMpd device) {
      After = [ "mpd.service" ];
      PartOf = [ "mpd.service" ];
    };
    Service = {
      Type = "simple";
      Restart = "on-failure";
      RestartSec = "5s";
      Environment = [
        "MPC_PLUS_SINGLE_DEVICE=${name}"
      ];
      ExecStart = "${lib.getExe cfg.package} notifications-daemon";
    };
  };
in
{
  options.custom.services.mpc-plus = {
    enable = lib.mkEnableOption "mpc-plus";
    package = lib.mkOption {
      type = lib.types.package;
      default = programCfg.package;
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [ (lib.hm.assertions.assertPlatform "custom.services.mpc-plus" pkgs lib.platforms.linux) ];
    systemd.user.services = lib.mapAttrs' (name: device:
      lib.nameValuePair "mpc-plus-${name}" (mkService name device)
    ) devicesWithNotifications;
  };
}
