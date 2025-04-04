{ config, pkgs, ... }:
{
  imports = [
    ./hardware
    ./disko.nix
    ./network-drives.nix
    ./peripherals.nix
    ../../nixos
    ../../nixos/desktop

    # Users
    ./bphenriques
  ];

  networking.hostName = "bphenriques-laptop";

  boot = {
    kernelPackages = pkgs.linuxPackages_6_12;

    initrd.systemd.enable = true; # Hibernation
    loader = {
      timeout = 0;  # The menu can be shown by pressing and holding a key before systemd-boot is launched.
      systemd-boot = {
        enable = true;
        editor = false;
        consoleMode = "1"; # bigger font in boot menu
        configurationLimit = 10;
      };
    };
  };

    # boot.loader.systemd-boot.windows: https://search.nixos.org/options?channel=unstable&show=boot.loader.systemd-boot.windows.%3Cname%3E.efiDeviceHandle&from=0&size=50&sort=relevance&type=packages&query=systemd-boot
  # FIXME: custom.boot.grub.windows.efiDevice = "38CB-E581";

  # Secrets
  sops.defaultSopsFile = ./secrets.yaml;
  sops.age.keyFile = "/var/lib/sops-nix/system-keys.txt";

  # Users
  users.mutableUsers = false;
  nix.settings.trusted-users = [ config.users.users.bphenriques.name ];

  system.stateVersion = "24.05"; # The release version of the first install of this system. Leave as it is!
}
