{ pkgs, lib, config, ... }:
let
  timer = {
    blank   = 60 * 10;
    lock    = 60 * 15;
    suspend = 60 * 30;
  };

  systemctl = lib.getExe' pkgs.systemd "systemctl";
  pidof = lib.getExe' pkgs.procps "pidof";
  niri = lib.getExe pkgs.niri;
  hyprlock = lib.getExe pkgs.hyprlock;

  suspend = "${systemctl} suspend";
  lock = ''${pidof} hyprlock || ${niri} msg action do-screen-transition --delay-ms 750 && ${hyprlock}'';
  power-off-monitors = "${niri} msg action power-off-monitors";
  power-on-monitors = "${niri} msg action power-on-monitors";
in
{
  services.swayidle = {
    enable = true;
    extraArgs = [ "-w" ]; # Wait for commands to complete
    timeouts = [
      { timeout = timer.blank;    command = power-off-monitors; }
      { timeout = timer.lock;     command = lock; }
      { timeout = timer.suspend;  command = suspend; }
    ];

    events = [
      { event = "before-sleep"; command = power-off-monitors; }
      { event = "after-resume"; command = power-on-monitors;  }
    ];
  };
}