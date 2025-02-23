{ pkgs, lib, config, ... }:
let
  cfg = config.custom.desktop-environment.idle;
in
{
  options.custom.desktop-environment.idle = {
    audio-inhibit = lib.mkEnableOption "disable idling if audio is playing" // {
      default = true;
    };

    timer = {
      blank   = lib.mkOption { type = lib.types.int; default = 60 * 10; };
      lock    = lib.mkOption { type = lib.types.int; default = 60 * 15; };
      suspend = lib.mkOption { type = lib.types.int; default = 60 * 30;};
    };
  };

  config = {
    # TODO ideal invariant: sleep > lock > suspend

    services.swayidle = {
      enable = true;
      extraArgs = [ "-w" ]; # Wait for commands to complete
      timeouts = [
        {
          timeout = cfg.timer.blank;
          command = config.custom.desktop-environment.compositor.power-off-monitors;
        }
        {
          timeout = cfg.timer.lock;
          command = config.custom.desktop-environment.session.lock;
        }
        {
          timeout = cfg.timer.suspend;
          command = config.custom.desktop-environment.session.suspend;
        }
      ];

      events = [
        { event = "before-sleep"; command = config.custom.desktop-environment.compositor.power-off-monitors; }
        { event = "after-resume"; command = config.custom.desktop-environment.compositor.power-on-monitors;  }
      ];
    };

    systemd.user.services.sway-audio-idle-inhibit = lib.mkIf cfg.audio-inhibit {
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