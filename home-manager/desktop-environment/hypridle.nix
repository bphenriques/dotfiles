{ pkgs, lib, config, self, ... }:
let
  niri = lib.getExe pkgs.niri;
  lock = ''${lib.getExe' pkgs.procps "pidof"} hyprlock || ${niri} msg action do-screen-transition --delay-ms 750 && ${lib.getExe pkgs.hyprlock}'';
  mkIcon = self.lib.builders.mkNerdFontIcon pkgs { textColor = config.lib.stylix.colors.withHashtag.base07; };
  notify = { msg, icon }: ''${lib.getExe pkgs.libnotify} --expire-time 5000 --icon "${icon}" --category "hypridle-notify" --hint string:x-canonical-private-synchronous:hypridle-notify --hint string:x-dunst-stack-tag:hypridle-notify "${msg}"'';
in
{
  services.hypridle = {
    enable = true;
    settings = {
      general = {
        lock_cmd = lock;
        unlock_cmd = "pkill --signal SIGUSR1 hyprlock";
        before_sleep_cmd = lock;
        after_sleep_cmd = "${niri} msg action power-on-monitors";
      };

      listener = [
        { timeout = 60 * 4;   on-timeout = notify { msg = "Turning off screen in 1 minute"; icon = (mkIcon "sleep" "󰒲"); }; }
        { timeout = 60 * 5;   on-timeout = "${niri} msg action power-off-monitors"; on-resume = "${niri} msg action power-on-monitors"; }
        { timeout = 60 * 10;  on-timeout = lock; }
        { timeout = 60 * 15;  on-timeout = "${lib.getExe' pkgs.systemd "systemctl"} suspend"; }
      ];
    };
  };
}
