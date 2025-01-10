{ pkgs, config, lib, self, ... }:

let
  pidof = lib.getExe' pkgs.procps "pidof";
  hyprlock = lib.getExe config.programs.hyprlock.package;
  niri = lib.getExe pkgs.niri;
  osd-brightness = lib.getExe self.pkgs.osd-brightness;
  systemctl = lib.getExe' pkgs.systemd "systemctl";
in
{
  services.swayidle = {
    enable = true;
    timeouts = [
      {
        timeout = 60 * 5;
        command = "${osd-brightness} dim >/dev/null 2>&1";
        resumeCommand = "${osd-brightness} restore >/dev/null 2>&1";
      }
      {
        timeout = 60 * 6;
        command = "${pidof} hyprlock || ${niri} msg action spawn -- ${hyprlock}";
      }
      {
        timeout = 60 * 10;
        command = "${systemctl} suspend";
      }
    ];

    events = [
      { event = "before-sleep"; command = "${niri} msg action power-off-monitors"; }
      { event = "after-resume"; command = "${niri} msg action power-on-monitors";  }
    ];
  };
}
