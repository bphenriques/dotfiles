{ config, pkgs, lib, ... }:
let
  shared = import ../shared.nix;
in
{
  imports = [
    ./hardware
    ./disko.nix
    ../../profiles/nixos
    ../../profiles/nixos/desktop

    # Users
    ./bphenriques

    # Temporary
    ../homelab/compute/datastores
    ../homelab/compute/services
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

  # Homelab integration
  networking.hosts = lib.mapAttrs' (name: ip: lib.nameValuePair ip [ name ]) shared.networks.homelab.hosts;
  custom.homelab.cifs = {
    enable = true;
    hostname = shared.networks.homelab.hosts.bruno-home-nas;
    mounts = {
      bphenriques = { gid = 5000; };
      media = { gid = 5001; };
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
