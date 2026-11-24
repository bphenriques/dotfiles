{ config, lib, pkgs, self, ... }:
let
  inherit (config.programs.quick-key.calendarSync) eventsFile tasksFile exec;
  # With an agent in Ask, a level's input sends its writes to the agent in plain words.
  hasAgent = config.programs.quick-key.settings.plugins ? ask;
  # What Ask tells the agent beside each message from that input; the conversation shows the text as typed.
  agentInput = desc: context: { inherit desc context; plugin = "ask"; };
  # Both levels' bars: the calendar status alone knows when the shared sync runs.
  sync = { key = "s"; desc = "Sync"; icon = self.lib.glyphs."md-sync"; status = "calendar"; format = "{updated}"; cmd = exec.sync; keepOpen = true; };
in
lib.mkIf pkgs.stdenv.hostPlatform.isLinux {
  programs.quick-key.calendarSync.enable = true;
  programs.quick-key.settings = {
    plugins.tasks = {
      file = tasksFile;
      inherit (exec) done reopen sync add;
      glyph = self.lib.glyphs."md-format_list_checks";
    };
    menu.header.left = [
      ({ key = "c"; desc = "Calendar"; status = "clock"; width = 960; view = "calendar"; header.right = [ sync ]; } // lib.optionalAttrs hasAgent { input = agentInput "Add an event..." "From the menu's Calendar input: an event to add, or an answer to your question"; })
    ];
  };

  my.quick-key.entries.tasks = [
    { key = "t"; desc = "Tasks"; status = "tasks"; list = "tasks"; input = if hasAgent then agentInput "Add a task..." "From the menu's Tasks input: a task to add, or an answer to your question" else { desc = "Add a task..."; }; header.right = [ sync ]; submenu = [
      { key = "s"; desc = "Sort"; action = "sort"; }
      { key = "c"; desc = "Done"; action = "completed"; }
    ]; }
  ];

  home.packages = [ (self.packages.calendar-cli.override { stateDir = dirOf eventsFile; }) ];
}
