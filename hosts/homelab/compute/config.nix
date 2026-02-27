{ config, pkgs, lib, ... }:
let
  homelabNetwork = import ../network.nix;
in
{
  imports = [
    # Base
    ./hardware
    ./disko.nix
    ./datastores
    ./services
    ./tasks
    ../../../config/nixos
    ../../../config/nixos/headless

    # Users
    ./bphenriques
  ];

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
  networking.hosts = lib.mapAttrs' (name: ip: lib.nameValuePair ip [ name ]) homelabNetwork.hosts;
  custom.fileSystems.homelab = {
    enable = true;
    hostname = homelabNetwork.hosts.bruno-home-nas;
    mounts = {
      bphenriques = { };
      media = { };
    };
  };

  # Podman for containers (romm, tinyauth, cleanuparr)
  virtualisation = {
    podman.enable = true;
    oci-containers.backend = "podman";
    containers.containersConf.settings.containers = {
      default_capabilities = [];
      pids_limit = 100;
      no_new_privileges = true;
    };
  };

  # Secrets
  sops.defaultSopsFile = ./secrets.yaml;
  sops.age.keyFile = "/var/lib/sops-nix/system-keys.txt";

  # Users
  users.mutableUsers = false;
  nix.settings.trusted-users = [ config.users.users.bphenriques.name ];

  system.stateVersion = "25.11"; # The release version of the first install of this system!
}
