{ pkgs, config, lib, ... }:
let
  cfg = config.custom.hardware.gamepad-8bitdo-ultimate2c;
in {
  options.custom.hardware.gamepad-8bitdo-ultimate2c = with lib.types; {
    enable = lib.mkEnableOption "8bitdo 2.4 Ultimate 2C Controller support";

    vendorId = lib.mkOption {
      type = str;
      description = "id of the product. You can find using lsusb: ID 2dc8:310a means 2dc8 as vendorId";
    };

    productId = lib.mkOption {
      type = str;
      description = "id of the product. You can find using lsusb: ID 2dc8:310a means 310a as productId";
    };
  };

  # Tip: When on: press and hold X/B and Home to power off. Then hold X/B to power on. The mode will be the one selected:
  # - X -> Xinput (displays as Xbox Controller)
  # - D -> DInput (displays as 8Bitdo Ultimate 2C Wireless)
  config = lib.mkIf cfg.enable {
    hardware.xpadneo.enable = true;   # Wireless Xbox(ish) gamepads (e.g., 8bitdo)
    services.udev.extraRules = ''
      ACTION=="add", \
        ATTRS{idVendor}=="${cfg.vendorId}", \
        ATTRS{idProduct}=="${cfg.productId}", \
        RUN+="${pkgs.kmod}/bin/modprobe xpad", \
        RUN+="${pkgs.bash}/bin/sh -c 'echo ${cfg.vendorId} ${cfg.productId} > /sys/bus/usb/drivers/xpad/new_id'"
    '';
  };
}

