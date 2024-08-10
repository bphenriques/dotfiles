{ pkgs, config, ... }:
{
  # Keyboard
  hardware.keyboard.zsa.enable = true;  # Required to flash or use live training online
  # Nuphy Air75 does not make sense. This ensures:
  # - Use the regular Windows key to open the menu (swap_opt_cmd which makes more sense setting to 1.. not 0...)
  # - Use Function Keys first over media keys (fnmode=0 or 2 works, 0 does not make sense...). If this does not work, press Fn+Tab+F to toggle in your Nuphy keyboard.
  # Check the flags with: modinfo -p hid_apple
  boot.extraModprobeConfig = "options hid_apple fnmode=2 swap_opt_cmd=0";

  # Touch Pad
  services.libinput = {
    enable = true;
    touchpad.naturalScrolling = true;
    touchpad.tapping = true;
  };

  ## Mouse - Using solaar and input-remapper to control my mouse's side buttons.
  modules.services.solaar.enable = true;
  modules.services.input-remapper.enable = true;
  home-manager.users.bphenriques = { config, ... }:
  {
    xdg.configFile = {
      "input-remapper/config.json".source = config.lib.file.mkOutOfStoreSymlink "${config.custom.dotfiles.directory}/home/input-remapper/config.json";
      "input-remapper/presets/Logitech G305/Media.json".source = config.lib.file.mkOutOfStoreSymlink "${config.custom.dotfiles.directory}/home/input-remapper/Media.json";
    };
  };
}