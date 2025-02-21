{ config, self, pkgs, lib, ... }:
let
  inherit (config.custom.desktop-environment) settings;

  screen-recorder = lib.getExe self.pkgs.screen-recorder;
  date = lib.getExe' pkgs.coreutils "date";

  destination = "$(${date} +'${settings.screen-recorder.directory}/${settings.screen-recorder.format}')";

  screen-audio        = ''${screen-recorder} screen-audio "${destination}"'';
  screen-no-audio     = ''${screen-recorder} screen-no-audio "${destination}"'';
  region-audio        = ''${screen-recorder} region-audio "${destination}"'';
  region-no-audio     = ''${screen-recorder} region-no-audio "${destination}"'';
  stop                = ''${screen-recorder} stop'';

  cmd = desc: cmd: { inherit desc cmd; };
  submenu = desc: submenu: { inherit desc submenu; };
in
{
  custom.desktop-environment.apps = {
    screen-recorder = { inherit screen-audio screen-no-audio region-audio region-no-audio stop; };
    menus.screen-recorder = self.lib.builders.writeDmenuScript pkgs {
      name = "screen-recorder-dmenu";
      # TODO: Menu start: 󰑋
      entries = [
        { label = "󰹑    Record screen (with audio)";  exec = screen-audio; }
        { label = "󰹑    Record screen (no audio)";    exec = screen-no-audio; }
        { label = "    Record region (with audio)";  exec = region-audio; }
        { label = "    Record region (no audio)";    exec = region-no-audio; }
        { label = "    Stop recording";              exec = stop; }
      ];
    };
  };

  custom.programs.wlr-which-key.menus.screen-recorder = {
    S = cmd "[S]top current recording" stop;
    s = submenu "Record [s]creen" {
      a = cmd "with [a]udio"  screen-audio;
      n = cmd "[n]o audio"    screen-no-audio;
    };
    r = submenu "Record [r]egion" {
      a = cmd "with [a]udio"  region-audio;
      n = cmd "[n]o audio"    region-no-audio;
    };
  };

  home.packages = [
    pkgs.wl-screenrec
    self.pkgs.screen-recorder
  ];
}