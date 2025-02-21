{ config, pkgs, self, lib, ... }:
let
  inherit (config.custom.desktop-environment) screenshots;
in
{
  xdg.configFile."swappy/config".text = lib.generators.toINI { } {
    Default = {
      save_dir = screenshots.directory;
      save_filename_format = screenshots.format;
      show_panel = true;
      early_exit = true;
      auto_save = true;
    };
  };
}