{ config, lib, pkgs, self, ... }:
let
  sshHost = "agent-alfredo-vm";
  package = self.packages.alfredo.override { inherit sshHost; };
  key = "Mod+Shift+Space";
in
{
  home.packages = [ package ];

  programs.quick-key.settings.plugins.ask = {
    title = "Alfredo";
    shortcut = key;
    glyph = self.lib.glyphs."fa-user_tie";
    ask = "ssh ${sshHost} alfredo acp";   # the API key never leaves the guest
    cwd = self.nixosConfigurations.${sshHost}.config.services.hermes-agent.workingDirectory;
    # Whatever he writes to the agenda shows here once synced, from any surface: after any of its tools that write (all but list_*).
    toolMatch = "^mcp__agenda__(?!list_)";
    toolRun = config.programs.quick-key.calendarSync.exec.sync;
    notify = "${lib.getExe pkgs.libnotify} --app-name=Alfredo --action=default=Open --hint=string:x-dunst-stack-tag:ask --";   # a click runs `default`; each answer replaces the last by its tag
    open = config.my.programs.terminal.execApp { title = "Alfredo"; cmd = lib.getExe package; };
  };

  my.quick-key.bar.ask = [ "ask" ];
  # The top level's catch-all.
  programs.quick-key.settings.menu.input = { desc = "Ask Alfredo..."; plugin = "ask"; };

  # Opens the menu with that input focused.
  my.programs.niri.bindings."${key} repeat=false" = ''spawn-sh "${config.programs.quick-key.exec.ipc} menu press Tab"'';
}
