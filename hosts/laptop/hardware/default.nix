{ pkgs, config, ... }:
{
  imports = [
    ./hardware-configuration.nix  # Output of nixos-generate-config --root /mnt
    ./amd.nix
    ./nvidia.nix
  ];

  # Networking
  networking.networkmanager.wifi.powersave = true;

  # Bluetooth
  hardware.bluetooth.enable = true;
  services.blueman.enable = true;

  # RAM
  zramSwap.enable = true; # Run zramctl to check how good memory is compressed

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
    # `top` but for GPUs. Very very useful to see which GPU is being used
    (pkgs.nvtopPackages.amd.override { nvidia = (builtins.elem "nvidia" config.services.xserver.videoDrivers); })

    pkgs.cheese     # Webcam
  ];
}
