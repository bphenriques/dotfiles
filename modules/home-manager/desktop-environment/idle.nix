{ pkgs, lib, config, ... }:
let
  cfg = config.custom.desktop-environment.idle;
in
{
  options.custom.desktop-environment.idle = {
    timer = {
      blank   = lib.mkOption { type = lib.types.int; default = 60 * 10; };
      lock    = lib.mkOption { type = lib.types.int; default = 60 * 15; };
      suspend = lib.mkOption { type = lib.types.int; default = 60 * 30;};
    };
  };

  config = {
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
  };
}