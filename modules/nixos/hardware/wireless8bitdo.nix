{ pkgs, config, lib, ... }:
let
  cfg = config.custom.hardware.wireless8bitdo;
in {
  options.custom.hardware.wireless8bitdo = with lib.types; {
    enable = lib.mkEnableOption "8bitdo controller support";

    vendorId = lib.mkOption {
      type = str;
      description = "id of the product. You can find using lsusb: ID 2dc8:301c means 2dc8 as vendorId";
    };

    productId = lib.mkOption {
      type = str;
      description = "id of the product. You can find using lsusb: ID 2dc8:301c means 301c as productId";
    };
  };

  config = lib.mkIf cfg.enable {
    services.udev.extraRules = ''
      ACTION=="add", \
        ATTRS{idVendor}=="${cfg.vendorId}", \
        ATTRS{idProduct}=="${cfg.productId}", \
        RUN+="${pkgs.kmod}/bin/modprobe xpad", \
        RUN+="${pkgs.bash}/bin/sh -c 'echo ${cfg.vendorId} ${cfg.productId} > /sys/bus/usb/drivers/xpad/new_id'"
    '';
  };
}