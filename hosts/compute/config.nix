{ config, pkgs, lib, self, ... }:
{
  imports = [
    # Base
    ./hardware
    ./disko.nix
    ./datastores
    ./services
    ./tasks
    ../../profiles/nixos
    ../../profiles/nixos/headless

    # Users
    ./bphenriques
  ];

  # Basic setup
  networking.hostName = "compute";
  boot = {
    kernelPackages = pkgs.linuxPackages_6_18;
    kernelParams = [ "systemd.debug_shell=1" ]; # TODO: remove after debugging
    loader.systemd-boot = {
      enable = true;
      editor = false;
      configurationLimit = 10;
    };
  };

  # Homelab integration
  networking.hosts = lib.mapAttrs' (name: ip: lib.nameValuePair ip [ name ]) self.shared.networks.main.hosts;
  custom.homelab.smb = {
    enable = true;
    hostname = self.shared.networks.main.hosts.bruno-home-nas;
    mounts = {
      bphenriques = { gid = 5000; };
      media = { gid = 5001; };
    };
  };
  custom.homelab.paths.users.bphenriques.root = lib.mkDefault config.custom.homelab.smb.mounts.bphenriques.localMount;

  # Podman for containers (tinyauth, cleanuparr, romm + romm-db)
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
