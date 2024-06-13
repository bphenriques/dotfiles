{ config, pkgs, ... }:

# TODO: should I check https://github.com/nix-community/nixos-anywhere-examples/blob/main/configuration.nix ?
{
  imports = [
    ./hardware-configuration.nix
    ../../nixos/config
    ./home.nix
    ./desktop-environment.nix
    ./disk-config.nix
  ];

  services.openssh.enable = true;
  users.users.root.openssh.authorizedKeys.keys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICFZfCSPN6cPfJKPU/cqEr7rCinDycRKWt2FmOiJdo/Y 4727729+bphenriques@users.noreply.github.com"
  ];

  # Basic settings
  networking.hostName = "bphenriques-laptop";
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
  boot.kernelPackages = pkgs.linuxPackages_latest;

  # Hardware
  ## Disk management
  services.fstrim.enable = true;              # Trim SSD because for some reason is not a default :shrug:

  ## Video Driver - Nvidia
  #services.xserver.videoDrivers = [ "nvidia" ];
  #hardware.nvidia.forceFullCompositionPipeline = true; # Fixes screen flickering
  #virtualisation.docker.enableNvidia = true;

  # The release version of the first install of this system. Leave as it is!
  system.stateVersion = "24.05";
}
