{ pkgs, config, ... }:
{
  # Zsa Keyboard - Required to flash or use live training online
  hardware.keyboard.zsa.enable = true;  # Required to flash or use live training online
  # Nuphy Air75 Keyboard (check the flags with `modinfo -p hid_apple`):
  # - Use the regular Windows key to open the menu (swap_opt_cmd which makes more sense setting to 1.. not 0...)
  # - Use Function Keys first over media keys (fnmode=0 or 2 works, 0 does not make sense...). If this does not work, press Fn+Tab+F to toggle in your Nuphy keyboard.
  boot.extraModprobeConfig = "options hid_apple fnmode=2 swap_opt_cmd=0";

  # Touchpad
  services.libinput = {
    enable = true;
    touchpad.naturalScrolling = false;
    touchpad.tapping = true;
  };

  # Fingerprint - run fprintd-enroll afterwards. Install `usbutils` and then run  `lsusb` to find the device.
  # Not enabled due to slow login: related with the order of pam.d/login which is not configurable atm.
  #services.fprintd = {
  #  enable = true;
  #  tod.enable = true;
  #  tod.driver = pkgs.libfprint-2-tod1-goodix-550a;
  #};

  custom.services.solaar.enable = true;

  hardware.xpadneo.enable = true; # Wireless Xbox(ish) gamepads. E.g., wireless 8bitdo
  hardware.xone.enable = true;    # Wired Xbox(ish) gamepads. E.g., wired 8bitdo

  # Other
  environment.systemPackages = [
    pkgs.cheese     # Webcam
  ];
}
