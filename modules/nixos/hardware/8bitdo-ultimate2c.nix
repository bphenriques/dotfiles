{ pkgs, config, lib, ... }:
let
  cfg = config.my.hardware.gamepad-8bitdo-ultimate2c;

  # Discoverable using lsusb. You will find 2dc8:310a.
  vendorId = "2dc8";
  productId = "310a";
in {
  options.my.hardware.gamepad-8bitdo-ultimate2c = {
    enable = lib.mkEnableOption "8bitdo 2.4 Ultimate 2C Controller support";
  };

  # Mode: hold X (Xinput) or D (DInput) with Home to power off, then the same button to power on.
  config = lib.mkIf cfg.enable {
    hardware.xpadneo.enable = true;   # Wireless Xbox(ish) gamepads (e.g., 8bitdo)

    services.udev.extraRules = ''
      ACTION=="add", \
        ATTRS{idVendor}=="${vendorId}", \
        ATTRS{idProduct}=="${productId}", \
        RUN+="${pkgs.kmod}/bin/modprobe xpad", \
        RUN+="${pkgs.bash}/bin/sh -c 'echo ${vendorId} ${productId} > /sys/bus/usb/drivers/xpad/new_id'"
    '';
  };
}

