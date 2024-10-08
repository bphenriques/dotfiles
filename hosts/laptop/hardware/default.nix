{ pkgs, config, ... }:

{
  imports = [
    ./hardware-configuration.nix   # Output of nixos-generate-config --root /mnt
    ./graphics.nix                 # AMD iGPU + Nvidia dGPU
    ./peripherals.nix     # Mouse / Keyboard / etc
  ];


  # Networking
  networking.networkmanager.wifi = {
    powersave = true;
    macAddress = "preserve";  # The default. I am fine as this laptop stays most of the times at home.
  };

  # Bluetooth
  # TODO: https://github.com/bashfulrobot/nixos/blob/main/modules/hw/bluetooth/default.nix
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
