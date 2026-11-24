{ pkgs, lib, config, self, ... }:
let
  niri = lib.getExe pkgs.niri;
  lock = ''${lib.getExe' pkgs.procps "pidof"} hyprlock || ${lib.getExe pkgs.hyprlock}'';
  mkIcon = self.lib.builders.mkNerdFontIcon { textColor = config.lib.stylix.colors.withHashtag.base07; };
  # The warning, with Keep awake as its one action: a click or Mod+N then RET keeps the screen on, as does quick-key's keep awake key (keepAwakeKey in quick-key.nix).
  idleWarning = pkgs.writeShellScript "idle-warning" ''
    action=$(${lib.getExe pkgs.libnotify} --app-name=Idle -i "${mkIcon "sleep" self.lib.glyphs."md-sleep"}" --category "hypridle-notify" \
      --expire-time 60000 --action "default=Keep awake" "Turning off screen in 1 minute" "Mod+I keeps it on" || true)
    [ "$action" = default ] && ${config.programs.quick-key.exec.ipc} keepAwake enable
  '';
in
lib.mkIf pkgs.stdenv.hostPlatform.isLinux {
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
        { timeout = 60 * 19;   on-timeout = "${idleWarning}"; }
        { timeout = 60 * 20;  on-timeout = "${niri} msg action power-off-monitors"; on-resume = "${niri} msg action power-on-monitors"; }
        { timeout = 60 * 25;  on-timeout = lock; }
        #{ timeout = 60 * 45;  on-timeout = "${lib.getExe' pkgs.systemd "systemctl"} suspend"; }
      ];
    };
  };
}
