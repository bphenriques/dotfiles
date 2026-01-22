{ config, pkgs, ... }:
{
  imports = [
    # Base
    ./hardware
    ./disko.nix
    ./services
    ./tasks
    ../../config/nixos
    ../../config/nixos/headless

    # Users
    ./bphenriques
  ];

  # TODO: https://blog.aldnav.com/blog/going-headless-with-nixos/

  # Basic setup
  networking.hostName = "compute";
  boot = {
    kernelPackages = pkgs.linuxPackages_6_18;
    loader.systemd-boot = {
      enable = true;
      editor = false;
      configurationLimit = 10;
    };
  };

  # Homelab integration
  custom.fileSystems.homelab.enable = true;
  custom.fileSystems.homelab.mounts = {
    bphenriques = { };
    media = { };
  };

  # Secrets
  sops.defaultSopsFile = ./secrets.yaml;
  sops.age.keyFile = "/var/lib/sops-nix/system-keys.txt";

  # Users
  users.mutableUsers = false;
  nix.settings.trusted-users = [ config.users.users.bphenriques.name ];

  system.stateVersion = "25.11"; # The release version of the first install of this system!
}
