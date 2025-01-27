{ config, pkgs, lib, self, ... }:
{
  imports = [
    ./hardware
    ./disko.nix
    ./network-drives.nix
    ./peripherals.nix
    ./zfs.nix
    ../../nixos
    ../../nixos/desktop-environment
    ../../nixos/desktop

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

  # Misc
  zramSwap.enable = true;         # Run zramctl to check how good memory is compressed

  # Gaming
  custom.programs.proton-run.enable = true;
  custom.programs.proton-run.defaultProtonDir = "/mnt/games/GlobalProton";

  # Secrets
  sops.defaultSopsFile = ./secrets.yaml;
  sops.age.keyFile = "/var/lib/sops-nix/system-keys.txt";

  # Users
  users.mutableUsers = false;
  nix.settings.trusted-users = [ config.users.users.bphenriques.name ];

  system.stateVersion = "24.05"; # The release version of the first install of this system. Leave as it is!
}
