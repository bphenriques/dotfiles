{ pkgs, ... }:
{
  imports = [
    ./hardware-configuration.nix  # Output of nixos-generate-config --root /mnt
    ./amd.nix
    ./nvidia.nix
    ./peripherals.nix
  ];

  # Misc
  services.fwupd.enable = true; # Updates firmwares: `fwupdmgr`.

  # Storage
  services.fstrim.enable = true;

  # Wifi
  networking.networkmanager.wifi.powersave = false; # Disable power saving to prevent frequent Wifi drops

  # Bluetooth
  hardware.bluetooth.enable = true;
  services.blueman.enable = true;

  # Battery
  services.upower = {
    enable = true;
    percentageLow = 30;
    percentageCritical = 20;
    percentageAction = 10;
    criticalPowerAction = "Hibernate";
  };

  environment.systemPackages = [
    pkgs.cheese     # Webcam
  ];
}
