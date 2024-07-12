{ config, ... }:
{
  ## Mouse - Using solaar and input-remapper to control my mouse's side buttons.
  modules.services = {
    solaar.enable = true;
    input-remapper.enable = true;
  };

  # Terrible Hack as workaround to readonly FS: https://github.com/sezanzeb/input-remapper/issues/663
  # mkOutOfStoreSymlink allows me to create a file outside of the store. I.e., to the actual file in the repo.
  # The path actually needs to be full due to: https://github.com/nix-community/home-manager/pull/1455#issuecomment-681041818
  home = { config, ... }: let
    hostDir = "/home/${config.custom.dotfiles.directory}/.dotfiles/host/desktop";
  in
  { # ensure config is within home-manager's context
    xdg.configFile = {
      "input-remapper/config.json".source = config.lib.file.mkOutOfStoreSymlink "${hostDir}/input-remapper/config.json";
      "input-remapper/presets/Logitech G305/Media.json".source = config.lib.file.mkOutOfStoreSymlink "${hostDir}/input-remapper/Media.json";
    };
  };

  # Nuphy Air75 does not make sense. This ensures:
  # - Use the regular Windows key to open the menu (swap_opt_cmd which makes more sense setting to 1.. not 0...)
  # - Use Function Keys first over media keys (fnmode=0 or 2 works, 0 does not make sense...). If this does not work, press Fn+Tab+F to toggle in your Nuphy keyboard.
  # Check the flags with: modinfo -p hid_apple
  boot.extraModprobeConfig = ''
    options hid_apple fnmode=2 swap_opt_cmd=0
  '';
}
