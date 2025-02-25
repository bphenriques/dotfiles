{ config, pkgs, lib, self, ... }:
{
  imports = [
    ./hardware
    ./disko.nix
    ./network-drives.nix
    ./peripherals.nix
    ./zfs.nix
    ../../nixos
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
  custom.boot.grub.windows.efiDevice = "38CB-E581";

  # Secrets
  sops.defaultSopsFile = ./secrets.yaml;
  sops.age.keyFile = "/var/lib/sops-nix/system-keys.txt";

  # Users
  users.mutableUsers = false;
  nix.settings.trusted-users = [ config.users.users.bphenriques.name ];

  system.stateVersion = "24.05"; # The release version of the first install of this system. Leave as it is!
}
