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

  my.programs.desktop-shell.integrations.ask.settings = {
    title = "Alfredo";
    glyph = self.lib.glyphs."fa-user_tie";
    ask = "${alfredo} ask";
    approve = "${alfredo} approve";
    stop = "${alfredo} stop";
    notify = "${lib.getExe pkgs.libnotify} --app-name=Alfredo --action=default=Open";   # dunst's left click runs `default`
    # Any image type: the adapter converts what it gets.
    pasteImage = let wl-paste = lib.getExe' pkgs.wl-clipboard "wl-paste"; in
      ''sh -c 'type=$(${wl-paste} --list-types | ${lib.getExe pkgs.gnugrep} -m1 "^image/") && ${wl-paste} --type "$type" > "$1"' paste-image'';
    open = config.my.programs.terminal.execApp { title = "Alfredo"; cmd = "${alfredo} open"; };
    shortcut = key;
  };

  my.programs.niri.bindings."${key} repeat=false" = ''spawn-sh "${config.my.programs.desktop-shell.exec.ipc} ask toggle"'';
}
