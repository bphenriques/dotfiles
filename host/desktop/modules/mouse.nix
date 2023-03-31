{ config, ... }:
let
  hostDir = "/home/${config.user.name}/.dotfiles/host/desktop";
in
{
  ## Mouse - Using solaar and input-remapper to control my mouse's side buttons.
  modules.services = {
    solaar.enable = true;
    input-remapper.enable = true;
  };

  # Terrible Hack as workaround to readonly FS: https://github.com/sezanzeb/input-remapper/issues/663
  # mkOutOfStoreSymlink allows me to create a file outside of the store. I.e., to the actual file in the repo.
  home = { config, ... }: { # ensure config is within home-manager's context
    xdg.configFile = {
      "input-remapper/config.json".source = config.lib.file.mkOutOfStoreSymlink "${hostDir}/input-remapper/config.json";
      "input-remapper/presets/Logitech G305/Media.json".source = config.lib.file.mkOutOfStoreSymlink "${hostDir}/input-remapper/Media.json";
    };
  };
}
