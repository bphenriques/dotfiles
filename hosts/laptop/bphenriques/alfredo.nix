{ config, lib, pkgs, self, ... }:
let
  sshHost = "agent-alfredo-vm";
  package = self.packages.alfredo.override { inherit sshHost; };
in
{
  home.packages = [ package ];

  programs.quick-key.settings.plugins.ask = {
    title = "Alfredo";
    shortcut = "Mod+Shift+Space";
    glyph = self.lib.glyphs."fa-user_tie";
    # ACP over SSH: his guest runs hermes-acp as its gateway runs Hermes (alfredo-acp), and the API key never leaves it.
    ask = "ssh ${sshHost} alfredo-acp";
    cwd = "/var/lib/hermes/workspace";
    # Whatever he writes to the agenda shows here once synced, from any surface: after any of its tools that write (all but list_*).
    toolMatch = "^mcp__agenda__(?!list_)";
    toolRun = config.programs.quick-key.calendarSync.exec.sync;
    notify = "${lib.getExe pkgs.libnotify} --app-name=Alfredo --action=default=Open";   # dunst's left click runs `default`
    open = config.my.programs.terminal.execApp { title = "Alfredo"; cmd = lib.getExe package; };
  };

  programs.quick-key.settings.menu.header.right = lib.mkBefore [ "ask" ];
  # The top level's catch-all.
  programs.quick-key.settings.menu.input = { desc = "Ask Alfredo"; plugin = "ask"; };

  # Opens the menu with that input focused.
  my.programs.niri.bindings."Mod+Shift+Space repeat=false" = ''spawn-sh "${config.programs.quick-key.exec.ipc} menu press Tab"'';
}
