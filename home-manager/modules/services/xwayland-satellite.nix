{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.custom.services.xwayland-satellite;
in
{
  options.custom.services.xwayland-satellite = {
    enable = mkEnableOption ''xwayland-satellite.'';
    displayId = mkOption {
      type = types.int;
      default = 21;
      description = ''ID of the display. Do not forget to set the environment variable in your Window Manager'';
    };
  };

  config = mkIf cfg.enable {
    systemd.user.services.xwayland-satellite = {
      Unit = {
        ConditionEnvironment = [ "WAYLAND_DISPLAY" ];
        PartOf = [ "graphical-session.target" ];
        After = [ "graphical-session.target" ];
        Requisite = [ "graphical-session.target" ];
        OnFailure = [ "xwayland-satellite-failure-report.service" ];
      };
      Install.WantedBy = [ config.wayland.systemd.target ];
      Service = {
        Type = "simple";
        ExecStart = "${lib.getExe pkgs.xwayland-satellite} :${toString cfg.displayId}";
        NotifyAccess = "all";
        StandardOutput = "journal";
        Restart = "on-failure";
      };
    };

    systemd.user.services.xwayland-satellite-failure-report = {
      Service = {
        Type = "oneshot";
        ExecStart = lib.escapeShellArgs [
          (lib.getExe pkgs.libnotify) "--urgency=critical" "xwayland-satellite" "Crashed... restarting..."
        ];
      };
    };
  };
}
