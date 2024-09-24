{ pkgs, config, ... }:

{
  imports = [
    ./hardware-configuration.nix   # Output of nixos-generate-config --root /mnt
    ./graphics.nix                 # AMD iGPU + Nvidia dGPU
    ./peripherals.nix     # Mouse / Keyboard / etc
  ];

  # Bluetooth
  hardware.bluetooth = {
    enable = true;
    powerOnBoot = true; # Power up the bluetooth controller on boot
    settings = {
      General = {
        Enable = "Source,Sink,Media,Socket"; # Enable A2DP Sink
      };
    };
  };
  services.blueman.enable = true;
}
