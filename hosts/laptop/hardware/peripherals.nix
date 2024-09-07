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

  # Fingerprint - run fprintd-enroll afterwards. usbutils the lsusb to find device
  # FIXME: slow login after this. Related with the order of pam.d/login which is not configurable.
  #services.fprintd = {
  #  enable = true;
  #  tod.enable = true;
  #  tod.driver = pkgs.libfprint-2-tod1-goodix-550a; #libfprint-2-tod1-goodix-550a ?
  #};

  # Mouse - Using solaar and input-remapper to control my mouse's side buttons.
  custom.services.solaar.enable = true;
  custom.services.input-remapper.enable = true;
}
