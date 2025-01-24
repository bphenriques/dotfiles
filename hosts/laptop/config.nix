{ config, pkgs, lib, self, ... }:
{
  imports = [
    ./hardware                        # CPU, graphics, peripherals, etc
    ./filesystem                      # Partitioning, etc
    ../../nixos                       # Default nixos settings
    ../../nixos/desktop-environment   # My desktop environment
    ../../nixos/desktop               # The usual desktop applications

    # Users
    ./bphenriques
  ];

  networking.hostName = "bphenriques-laptop";

  boot = {
    supportedFilesystems.zfs = true;
    kernelPackages = pkgs.linuxPackages_6_12;

    loader.grub = {
      enable = true;
      efiSupport = true;
      efiInstallAsRemovable = true;
      configurationLimit = 10;
    };
  };
  custom.boot.grub.windowsEfiDevice = "38CB-E581";

  # Gaming
  custom.programs.proton-run.enable = true;
  custom.programs.proton-run.defaultProtonDir = "/mnt/games/GlobalProton";

  # System-wide secrets
  sops.defaultSopsFile = ./secrets.yaml;
  sops.age.keyFile = "/var/lib/sops-nix/system-keys.txt";

  # Users and groups
  users.mutableUsers = false;

  system.stateVersion = "24.05"; # The release version of the first install of this system. Leave as it is!
}

