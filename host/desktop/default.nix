{ config, pkgs, ... }:

{
  imports = [
    ./hardware-configuration.nix
    ./network-disks.nix
    ./peripherals.nix
    ../../nixos
    ./home.nix
    ./secrets.nix
  ];

  # Basic settings
  networking.hostName = "bphenriques-desktop";
  user.name = "bphenriques";

  # Points to NAS locations
  user.musicDir = "/home/${config.user.name}/media/music/library";
  user.romsDir = "/home/${config.user.name}/media/gaming/emulation/roms";

  user.protonDefaultPrefixDir = "/mnt/data/Games/Other";

  # Bootloader
  boot.loader.grub = {
    enable = true;
    device = "nodev";
    configurationLimit = 5;
  };

  # Latest kernel (aka the one pinned under flake.lock)
  # boot.kernelPackages = pkgs.linuxPackages_latest;

  # Hardware
  ## Disk management
  services.fstrim.enable = true;              # Trim SSD because for some reason is not a default :shrug:
  boot.supportedFilesystems = [ "ntfs" ];     # Support regular Windows FS

  ## Video Driver - Nvidia
  services.xserver.videoDrivers = [ "nvidia" ];
  hardware.nvidia.forceFullCompositionPipeline = true; # Fixes screen flickering
  virtualisation.docker.enableNvidia = true;

  # The release version of the first install of this system. Leave as it is!
  system.stateVersion = "22.11";
}
