{ config, lib, pkgs, self, ... }:
let
  inherit (config.programs.quick-key.calendarSync) eventsFile tasksFile exec;
  # With an agent in the Ask panel, a level takes its writes in plain words; its adapter syncs what he changes, and the level redrawing shows it.
  hasAgent = config.programs.quick-key.settings.plugins ? ask;
  agentInput = desc: format: { inherit desc format; plugin = "ask"; };
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
      ({ key = "c"; desc = "Calendar"; status = "clock"; width = 960; view = "calendar"; submenu = [
        { key = "s"; desc = "Sync"; icon = self.lib.glyphs."md-sync"; status = "calendar"; format = "|{updated}"; cmd = exec.sync; keepOpen = true; }
      ]; } // lib.optionalAttrs hasAgent { input = agentInput "Add an event" "Evento: {text}"; })
    ];
    menu.entries = lib.mkOrder 800 [
      { key = "t"; desc = "Tasks"; status = "tasks"; list = "tasks"; input = if hasAgent then agentInput "Add a task" "Tarefa: {text}" else { desc = "Add a task"; }; submenu = [
        { key = "/"; desc = "All tasks..."; action = "pick"; }
        { key = "s"; desc = "Sync"; icon = self.lib.glyphs."md-sync"; status = "tasks"; format = "|{updated}"; cmd = exec.sync; keepOpen = true; }
      ]; }
    ];
  };

  home.packages = [ (self.packages.calendar-cli.override { stateDir = dirOf eventsFile; }) ];
}
