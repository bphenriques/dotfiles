{ config, lib, pkgs, osConfig, self, ... }:
let
  package = self.packages.alfredo.override {
    sshHost = "agent-alfredo-vm";
    apiPort = osConfig.fleet.ai.assistantEndpoint.port;
  };
  alfredo = lib.getExe package;
  key = "Mod+Shift+Space";
in
{
  home.packages = [ package ];

  programs.quick-key.settings.plugins.ask = {
    title = "Alfredo";
    glyph = self.lib.glyphs."fa-user_tie";
    ask = "${alfredo} ask";
    notify = "${lib.getExe pkgs.libnotify} --app-name=Alfredo --action=default=Open";   # dunst's left click runs `default`
    open = config.my.programs.terminal.execApp { title = "Alfredo"; cmd = "${alfredo} open"; };
    shortcut = key;
  };

  my.programs.niri.bindings."${key} repeat=false" = ''spawn-sh "${config.programs.quick-key.exec.ipc} ask activate"'';
}
