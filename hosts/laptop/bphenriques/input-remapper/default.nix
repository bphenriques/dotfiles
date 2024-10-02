{ config, pkgs, ... }:
let
  basePath = "${config.custom.dotfiles.directory}/hosts/laptop/bphenriques/input-remapper";
in
{
  # Workaround to readonly FS: https://github.com/sezanzeb/input-remapper/issues/663
  xdg.configFile = {
    "input-remapper/config.json".source = config.lib.file.mkOutOfStoreSymlink "${basePath}/config.json";
    "input-remapper/presets/Logitech G305/Media.json".source = config.lib.file.mkOutOfStoreSymlink "${basePath}/Media.json";
  };
}
