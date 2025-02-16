{ pkgs, config, ... }:
{
  # Keyboards
  hardware.keyboard.zsa.enable = true;                                    # ZSA Keyboard
  boot.extraModprobeConfig = "options hid_apple fnmode=2 swap_opt_cmd=0"; # Nuphy Air75 (check the flags with `modinfo -p hid_apple`)

  # Mouse
  hardware.logitech.wireless.enable = true;                               # Wireless logitch devices
  hardware.logitech.wireless.enableGraphical = true;

  # Gamepads
  hardware.xone.enable = true;                                            # Wired Xbox(ish) gamepads (e.g., 8bitdo)
  custom.hardware.gamepad-8bitdo-ultimate2c = {
    vendorId = "2dc8";
    productId = "301c";
  };
}
