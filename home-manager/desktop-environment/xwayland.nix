{ config, pkgs, lib, ... }:
{
  systemd.user.services.xwayland-satellite = {
    Unit = {
      PartOf = [ "graphical-session.target" ];
      After = [ "graphical-session.target" ];
      Requisite = [ "graphical-session.target" ];
      OnFailure = [ "xwayland-satellite-failure-report.service" ];
    };
    Install.WantedBy = [ "graphical-session.target" ]; # FIXME: After config.wayland.systemd.target? Check the config...
    Service = {
      Type = "simple";
      ExecStart = "${lib.getExe pkgs.xwayland-satellite} :21"; #wa
      NotifyAccess = "all";
      StandardOutput = "journal";
      Restart = "on-failure";
    };
  };

  systemd.user.services.xwayland-satellite-failure-report = {
    Service = {
      Type = "oneshot";
      ExecStart = lib.escapeShellArgs [
        (lib.getExe pkgs.libnotify) "--urgency=critical" "xwayland-satellite" "Crashed and restarting..."
      ];
    };
  };
}