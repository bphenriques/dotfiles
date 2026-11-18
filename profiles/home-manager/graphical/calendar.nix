{ config, lib, pkgs, self, ... }:
let
  inherit (config.programs.quick-key.calendarSync) eventsFile tasksFile exec;
in
lib.mkIf pkgs.stdenv.hostPlatform.isLinux {
  programs.quick-key.calendarSync.enable = true;
  programs.quick-key.settings = {
    plugins.tasks = {
      file = tasksFile;
      inherit (exec) done reopen sync;
      add = lib.mkDefault exec.add;
      glyph = self.lib.glyphs."md-format_list_checks";
    };
    menu.header.left = [
      { key = "c"; desc = "Calendar"; status = "clock"; width = 960; view = "calendar"; submenu = [
        { key = "s"; desc = "Sync"; icon = self.lib.glyphs."md-sync"; status = "calendar"; format = "|{updated}"; cmd = exec.sync; keepOpen = true; }
      ]; }
    ];
    menu.entries = lib.mkOrder 800 [
      { key = "t"; desc = "Tasks"; status = "tasks"; list = "tasks"; input = { key = "a"; desc = "Add a task"; }; submenu = [
        { key = "space"; desc = "All tasks..."; action = "pick"; }
        { key = "s"; desc = "Sync"; icon = self.lib.glyphs."md-sync"; status = "tasks"; format = "|{updated}"; cmd = exec.sync; keepOpen = true; }
      ]; }
    ];
  };

  home.packages = [ (self.packages.calendar-cli.override { stateDir = dirOf eventsFile; }) ];
}
