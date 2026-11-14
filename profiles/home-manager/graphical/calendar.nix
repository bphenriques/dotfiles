{ config, lib, pkgs, self, ... }:
let
  inherit (config.programs.quickkey.calendarSync) eventsFile;
in
lib.mkIf pkgs.stdenv.hostPlatform.isLinux {
  programs.quickkey.calendarSync.enable = true;
  programs.quickkey.settings = {
    menu.entries = lib.mkOrder 800 [
      { key = "c"; desc = "Calendar"; status = "calendar"; width = 960; view = "calendar"; submenu = [
        { key = "s"; desc = "Sync"; icon = self.lib.glyphs."md-sync"; cmd = "${lib.getExe' pkgs.systemd "systemctl"} --user start --no-block quickkey-calendar-sync.service"; keepOpen = true; }
      ]; }
    ];
  };

  # khal and ikhal in the terminal, on the config calendar sync writes, so they show what the shell does.
  home.packages = map (name: pkgs.writeShellApplication {
    inherit name;
    runtimeInputs = [ pkgs.khal ];
    text = ''exec ${name} -c "${dirOf eventsFile}/khal.conf" "$@"'';
  }) [ "khal" "ikhal" ];
}
