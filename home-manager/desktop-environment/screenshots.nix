{ config, pkgs, self, lib, ... }:
let
  inherit (config.custom.desktop-environment) settings;

  screenshots = lib.getExe self.pkgs.screenshots;
  date = lib.getExe' pkgs.coreutils "date";

  destination = "$(${date} +'${settings.screenshots.directory}/${settings.screenshots.format}')";

  screen      = ''${screenshots} screen "${destination}"'';
  screen-copy = ''${screenshots} screen-copy "${destination}"'';
  screen-edit = ''${screenshots} screen-edit "${destination}"'';

  region      = ''${screenshots} region "${destination}"'';
  region-copy = ''${screenshots} region-copy "${destination}"'';
  region-edit = ''${screenshots} region-edit "${destination}"'';

  cmd = desc: cmd: { inherit desc cmd; };
  submenu = desc: submenu: { inherit desc submenu; };
in
{
  custom.desktop-environment.apps.screenshot = { inherit screen screen-copy screen-edit region region-copy region-edit; };
  custom.programs.wlr-which-key.menus.screenshot = {
    s = submenu "[s]creen" {
      s = cmd "[s]ave"  screen;
      c = cmd "[c]opy"  screen-copy;
      e = cmd "[e]dit"  screen-edit;
    };
    r = submenu "[r]egion" {
      s = cmd "[s]ave"  region;
      c = cmd "[c]opy"  region-copy;
      e = cmd "[e]dit"  region-edit;
    };
  };

  xdg.configFile."swappy/config".text = lib.generators.toINI { } {
    Default = {
      save_dir = settings.screenshots.directory;
      save_filename_format = settings.screenshots.format;
      show_panel = true;
      early_exit = true;
      auto_save = true;
    };
  };
}