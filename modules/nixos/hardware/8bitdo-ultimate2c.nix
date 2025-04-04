{ pkgs, config, lib, ... }:
let
  cfg = config.custom.hardware.gamepad-8bitdo-ultimate2c;

  # Discoverable using lsusb. You will find 2dc8:310a.
  vendorId = "2dc8";
  productId = "310a";
in {
  options.custom.hardware.gamepad-8bitdo-ultimate2c = with lib.types; {
    enable = lib.mkEnableOption "8bitdo 2.4 Ultimate 2C Controller support";
  };

  # Tip: When on: press and hold X/B and Home to power off. Then hold X/B to power on. The mode will be the one selected:
  # - X -> Xinput (displays as Xbox Controller)
  # - D -> DInput (displays as 8Bitdo Ultimate 2C Wireless)
  config = lib.mkIf cfg.enable {
    hardware.xpadneo.enable = true;   # Wireless Xbox(ish) gamepads (e.g., 8bitdo)

    # FIXME: This should be done automatically by Kernel 6.12
    services.udev.extraRules = ''
      ACTION=="add", \
        ATTRS{idVendor}=="${vendorId}", \
        ATTRS{idProduct}=="${productId}", \
        RUN+="${pkgs.kmod}/bin/modprobe xpad", \
        RUN+="${pkgs.bash}/bin/sh -c 'echo ${vendorId} ${productId} > /sys/bus/usb/drivers/xpad/new_id'"
    '';
  };
}

