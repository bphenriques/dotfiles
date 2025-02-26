{ config, lib, pkgs, ... }:
let
  cfg = config.custom.services.sway-audio-idle-inhibit;
in
{
  options.custom.services.sway-audio-idle-inhibit = with lib.types; {
    enable = lib.mkEnableOption ''sway-audio-idle-inhibit'' // {
      default = config.services.swayidle.enable;
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [ (lib.hm.assertions.assertPlatform "custom.services.sway-audio-idle-inhibit" pkgs lib.platforms.linux) ];

    systemd.user.services.sway-audio-idle-inhibit = lib.mkIf cfg.enable {
      Unit = {
        Description = "Prevents swayidle from sleeping while any application is outputting or receiving audio.";
        PartOf = [ "swayidle.service" ];
        After = [ "swayidle.service" ];
      };
      Install.WantedBy = [ config.wayland.systemd.target ];
      Service = {
        Type = "simple";
        ExecStart = lib.getExe pkgs.sway-audio-idle-inhibit;
        Restart = "on-failure";
      };
    };
  };
}
