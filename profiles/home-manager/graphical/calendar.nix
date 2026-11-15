{ config, lib, pkgs, self, ... }:
let
  inherit (config.programs.quick-key.calendarSync) eventsFile exec;
in
lib.mkIf pkgs.stdenv.hostPlatform.isLinux {
  programs.quick-key.calendarSync.enable = true;
  programs.quick-key.settings = {
    menu.entries = lib.mkOrder 800 [
      { key = "c"; desc = "Calendar"; status = "calendar"; width = 960; view = "calendar"; submenu = [
        { key = "s"; desc = "Sync"; icon = self.lib.glyphs."md-sync"; cmd = exec.sync; keepOpen = true; }
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
