{ config, pkgs, ... }:
let
  groups = config.users.groups;
  users = config.users.users;
in
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

    # boot.loader.systemd-boot.windows: https://search.nixos.org/options?channel=unstable&show=boot.loader.systemd-boot.windows.%3Cname%3E.efiDeviceHandle&from=0&size=50&sort=relevance&type=packages&query=systemd-boot
    loader.systemd-boot = {
      enable = true;
      editor = false;
      configurationLimit = 10;
    };
  };
  disko.devices.disk.vda.imageSize = "30G";

  # FIXME: custom.boot.grub.windows.efiDevice = "38CB-E581";

  # Secrets
  sops.defaultSopsFile = ./secrets.yaml;
  sops.age.keyFile = "/var/lib/sops-nix/system-keys.txt";

  # Users
  users.mutableUsers = false;
  nix.settings.trusted-users = [ config.users.users.bphenriques.name ];

  
  # systemd.tmpfiles.rules = [
  #  "z /mnt/games             0775 root                        ${groups.users.name}"
  #  "z /mnt/bphenriques       0700 ${users.bphenriques.name}   ${groups.users.name}"
  # ];

  system.stateVersion = "24.05"; # The release version of the first install of this system. Leave as it is!
}
