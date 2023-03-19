{ config, pkgs, ... }:

{
  imports = [ ./hardware-configuration.nix ];

  # Grub Bootloader
  boot.loader.grub = {
    enable = true;
    device = "/dev/sda";
    useOSProber = true;
  };

  # Latest kernel
  boot.kernelPackages = pkgs.linuxPackages_latest;

  # Disk management
  services.fstrim.enable = true;              # Trim SSD because for some reason is not a default :shrug:
  boot.supportedFilesystems = [ "ntfs" ];     # Support regular Windows FS

  # Video Driver
  services.xserver.videoDrivers = [ "nvidia" ];
  hardware.nvidia.forceFullCompositionPipeline = true; # Fixes screen flickering

  # Basic settings
  user.name = "bphenriques";
  networking.hostName = "bphenriques-desktop";

  # The release version of the first install of this system. Leave as it is!
  system.stateVersion = "22.11";
}
