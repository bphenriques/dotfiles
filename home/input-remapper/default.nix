{ config, pkgs, ... }:
{
  # Workaround to readonly FS: https://github.com/sezanzeb/input-remapper/issues/663
  xdg.configFile = {
    "input-remapper/config.json".source = config.lib.file.mkOutOfStoreSymlink "${config.custom.dotfiles.directory}/home/input-remapper/config.json";
    "input-remapper/presets/Logitech G305/Media.json".source = config.lib.file.mkOutOfStoreSymlink "${config.custom.dotfiles.directory}/home/input-remapper/Media.json";
  };
}
