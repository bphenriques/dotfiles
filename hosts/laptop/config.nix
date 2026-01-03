{ config, pkgs, ... }:
{
  imports = [
    ./hardware
    ./disko.nix
    ./network-drives.nix
    ./peripherals.nix
    ../../config/nixos
    ../../config/nixos/desktop
    ../../config/nixos/headless/services

    # Users
    ./bphenriques
  ];

  networking.hostName = "bphenriques-laptop";

  boot = {
    kernelPackages = pkgs.linuxPackages_6_18;

    initrd.systemd.enable = true; # Hibernation
    loader = {
      timeout = 0;  # The menu can be shown by pressing and holding a key before systemd-boot is launched.
      systemd-boot = {
        enable = true;
        editor = false;
        consoleMode = "max";
        configurationLimit = 10;
        windows."Windows" = {
          title = "Windows";
          efiDeviceHandle = "HD0b";
        };
      };
    };
  };

  # Secrets
  sops.defaultSopsFile = ./secrets.yaml;
  sops.age.keyFile = "/var/lib/sops-nix/system-keys.txt";

  # Users
  users.mutableUsers = false;
  nix.settings.trusted-users = [ config.users.users.bphenriques.name ];

  system.stateVersion = "24.05"; # The release version of the first install of this system. Leave as it is!
}
