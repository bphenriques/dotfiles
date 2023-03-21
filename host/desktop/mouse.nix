{ config, pkgs, ... }:
let
  hostDir = "/home/${config.user.name}/.dotfiles/host/desktop";
in
{
  ## Mouse - Remap the keys to control media buttons using input-remapper.
  hardware.logitech.wireless = {          # Compatibility with logitech devices.
    enable = true;
    enableGraphical = true;               # Installs Solaar. Ensure profile is off to allow key remapping.
  };

  services.input-remapper.enable = true;
   # Terrible Hack as workaround to https://github.com/sezanzeb/input-remapper/issues/663
   home-manager.users.${config.user.name} = { config, ... }: { # ensure config is within home-manager's context
    xdg.configFile = {
      "input-remapper/config.json".source = config.lib.file.mkOutOfStoreSymlink "${hostDir}/input-remapper/config.json";
      "input-remapper/presets/Logitech G305/Media.json".source = config.lib.file.mkOutOfStoreSymlink "${hostDir}/input-remapper/Media.json";
    };
  };
}
