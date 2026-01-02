{ config, pkgs, ... }:
{
  imports = [
    ./hardware
    ./disko.nix
    ./network-drives.nix
    ../../config/nixos

    # Users
    ./bphenriques
  ];

  # Core
  networking.hostName = "compute";
  boot.kernelPackages = pkgs.linuxPackages_6_18;

  # Secrets
  sops.defaultSopsFile = ./secrets.yaml;
  sops.age.keyFile = "/var/lib/sops-nix/system-keys.txt";

  # Users
  users.mutableUsers = false;
  nix.settings.trusted-users = [ config.users.users.bphenriques.name ];

  # Peripherals
  boot.extraModprobeConfig = "options hid_apple fnmode=2 swap_opt_cmd=0"; # Nuphy Air75 (check the flags with `modinfo -p hid_apple`)

  system.stateVersion = "24.05"; # The release version of the first install of this system. Leave as it is!
}
