{ pkgs, config, ... }:
{
  hardware.keyboard.zsa.enable = true;                                    # ZSA Keyboard
  boot.extraModprobeConfig = "options hid_apple fnmode=2 swap_opt_cmd=0"; # Nuphy Air75 (check the flags with `modinfo -p hid_apple`)
  hardware.xone.enable = true;                                            # Wired Xbox(ish) gamepads. E.g., wired 8bitdo
  hardware.logitech.wireless.enable = true;                               # Wireless logitch devices
  hardware.logitech.wireless.enableGraphical = true;
}
