{ pkgs, lib, config, ... }:
let
  niri = lib.getExe pkgs.niri;
  lock = ''${lib.getExe' pkgs.procps "pidof"} hyprlock || ${niri} msg action do-screen-transition --delay-ms 750 && ${lib.getExe pkgs.hyprlock}'';
in
{
  services.swayidle = {
    enable = true;
    extraArgs = [ "-w" ]; # Wait for commands to complete
    timeouts = [
      { timeout = 60 * 10;    command = "${niri} msg action power-off-monitors"; }
      { timeout = 60 * 15;     command = lock; }
      { timeout = 60 * 30;  command = "${lib.getExe' pkgs.systemd "systemctl"} suspend"; }
    ];

    events = [
      { event = "before-sleep"; command = lock; }
      { event = "after-resume"; command = "${niri} msg action power-on-monitors";  }
    ];
  };
}