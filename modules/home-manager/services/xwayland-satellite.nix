{ config, lib, pkgs, ... }:

let
  cfg = config.custom.services.xwayland-satellite;
in
{
  # TODO: Deprecate as Niri integrates with it.
  options.custom.services.xwayland-satellite = with lib.types; {
    enable = lib.mkEnableOption ''xwayland-satellite'';
    displayId = lib.mkOption {
      type = int;
      default = 21;
      description = ''ID of the display. Do not forget to set the environment variable in your Window Manager'';
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [ (lib.hm.assertions.assertPlatform "custom.services.xwayland-satellite" pkgs lib.platforms.linux) ];

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

    custom.programs.niri.environment.DISPLAY = ":${toString cfg.displayId}";
  };
}
