{ pkgs, config, lib, self, ... }:

let
  pidof = lib.getExe' pkgs.procps "pidof";
  hyprlock = lib.getExe config.programs.hyprlock.package;
  niri = lib.getExe pkgs.niri;
  brightness = lib.getExe self.pkgs.brightness-osd;
  systemctl = lib.getExe' pkgs.systemd "systemctl";
in
{
  services.swayidle = {
    enable = true;
    extraArgs = [ "-w" ]; # Wait for commands to complete
    timeouts = [
      {
        timeout = 60 * 5;
        command = "${brightness} dim >/dev/null 2>&1";
        resumeCommand = "${brightness} restore >/dev/null 2>&1";
      }
      {
        timeout = 60 * 10;
        command = "${pidof} hyprlock || ${niri} msg action spawn -- ${hyprlock}";
      }
      {
        timeout = 60 * 15;
        command = "${systemctl} suspend";
      }
    ];

    events = [
      { event = "before-sleep"; command = "${niri} msg action power-off-monitors"; }
      { event = "after-resume"; command = "${niri} msg action power-on-monitors";  }
    ];
  };
}
