{ config, lib, pkgs, ... }:
let
  cfg = config.modules.services.input-remapper;
in
{
  options.modules.services.input-remapper = {
    enable = lib.mkEnableOption ''input-remapper one-shot service to reload the profiles.'';
  };

  config = lib.mkIf cfg.enable {
    services.input-remapper = {
      enable = true;
      #enableUdevRules = true; # Enable built-in udev rules for when Xbox Controller is plugged in/connected through sunshine
    };
    systemd.user.services.input-remapper-reload = { # Fix for https://github.com/sezanzeb/input-remapper/issues/653
      enable = true;
      description = "Loadss input-remapper profiles";
      wantedBy = ["graphical-session.target"];
      after = ["graphical-session.target"];
      preStart = "/run/current-system/sw/bin/sleep 15";  # Minor hack that I can live with.
      script = "${pkgs.input-remapper}/bin/input-remapper-control --command stop-all && ${pkgs.input-remapper}/bin/input-remapper-control --command autoload";
    };
  };
}
