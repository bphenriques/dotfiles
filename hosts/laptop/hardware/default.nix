{ pkgs, config, ... }:

{
  imports = [
    ./hardware-configuration.nix   # Output of nixos-generate-config --root /mnt
    ./graphics.nix                 # AMD iGPU + Nvidia dGPU
    ./peripherals.nix     # Mouse / Keyboard / etc
  ];

  # Bluetooth
  # TODO review compatability with external headsets:
  # - https://github.com/bbigras/nix-config/blob/master/hardware/bluetooth.nix
  # - https://github.com/wimpysworld/nix-config/blob/main/nixos/_mixins/features/bluetooth/default.nix#L3
  hardware.bluetooth = {
    enable = true;
    powerOnBoot = true; # Power up the bluetooth controller on boot
  };
  services.blueman.enable = true;
}
