{ config, pkgs, ... }:

{
  imports = [
    ./hardware-configuration.nix
    ./modules/mouse.nix
    ../../nixos
    ./home.nix
  ];

  # Basic settings
  user.name = "bphenriques";
  user.musicDir = "/mnt/data/Media/Dropbox/Library/";
  networking.hostName = "bphenriques-desktop";

  # Bootloader
  boot.loader.grub = {
    enable = true;
    device = "/dev/sda";
    useOSProber = true;
  };

  # Latest kernel (aka the one pinned under flake.lock)
  boot.kernelPackages = pkgs.linuxPackages_latest;

  # Hardware
  ## Disk management
  services.fstrim.enable = true;              # Trim SSD because for some reason is not a default :shrug:
  boot.supportedFilesystems = [ "ntfs" ];     # Support regular Windows FS

  ## Video Driver
  services.xserver.videoDrivers = [ "nvidia" ];
  hardware.nvidia.forceFullCompositionPipeline = true; # Fixes screen flickering

  # The release version of the first install of this system. Leave as it is!
  system.stateVersion = "22.11";
}
