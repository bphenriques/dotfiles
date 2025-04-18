{ pkgs, ... }:
{
  imports = [
    ./hardware-configuration.nix  # Output of nixos-generate-config --root /mnt
    ./amd.nix
    ./nvidia.nix
  ];

  # Bluetooth
  hardware.bluetooth.enable = true;
  services.blueman.enable = true;

   # Battery
  services.upower = {
    enable = true;
    percentageLow = 30;
    percentageCritical = 20;
    percentageAction = 10;
    criticalPowerAction = "PowerOff";
  };

  # Touchpad
  services.libinput = {
    enable = true;
    touchpad.naturalScrolling = false;
    touchpad.tapping = true;
  };

  environment.systemPackages = [
    pkgs.cheese     # Webcam
  ];
}
