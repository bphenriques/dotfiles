_:
{
  # Keyboards
  hardware.keyboard.zsa.enable = true;                                    # ZSA Keyboard
  boot.extraModprobeConfig = "options hid_apple fnmode=2 swap_opt_cmd=0"; # Nuphy Air75 (check the flags with `modinfo -p hid_apple`)

  # Mouse
  programs.solaar = {
    enable = true;
    userService.enable = true;
    userService.extraArgs = [ "--restart-on-wake-up" ]; # https://github.com/pwr-Solaar/Solaar/issues/2024
  };

  # Gamepads
  hardware.xone.enable = true;                        # Wired Xbox(ish) gamepads (e.g., 8bitdo)
  custom.hardware.gamepad-8bitdo-ultimate2c.enable = true;
}
