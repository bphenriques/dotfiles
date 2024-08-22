{ pkgs, config, ... }:

{
  # Bluetooth
  # TODO: https://github.com/bbigras/nix-config/blob/master/hardware/bluetooth.nix
  # TODO: https://github.com/wimpysworld/nix-config/blob/main/nixos/_mixins/features/bluetooth/default.nix#L3
  # TODO: Set extra-hosts: https://github.com/wimpysworld/nix-config/blob/main/nixos/_mixins/features/network/default.nix#L70
  hardware.bluetooth = {
    enable = true;
    powerOnBoot = true; # Power up the bluetooth controller on boot
  };
  services.blueman.enable = true;

  # Fingerprint - run fprintd-enroll afterwards
  services.fprintd.enable = true;

  # FIXME:
  #  systemd.network.wait-online.enable = false;
  #  boot.initrd.systemd.network.wait-online.enable = false;
  #  networking.wireless.dbusControlled = false;

  # FIXME: Battery stuff: https://github.com/bbigras/nix-config/blob/master/hosts/laptop/default.nix#L173
}
