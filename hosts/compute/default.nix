{ config, pkgs, lib, self, private, ... }:
{
  imports = [
    # Base
    ./hardware
    ./disko.nix
    ./datastores
    ./services
    ./monitoring
    ./tasks
    ../../profiles/nixos
    ../../profiles/nixos/headless
    ../../profiles/nixos/homelab-smb-client.nix

    # Users
    ./bphenriques
  ];

  custom.fleet = import ../shared.nix;

  # Basic setup
  networking.hostName = "compute";
  boot = {
    kernelPackages = pkgs.linuxPackages_7_0;
    loader.systemd-boot = {
      enable = true;
      editor = false;
      configurationLimit = 10;
    };
  };

  # Homelab integration
  custom.homelab.smb.mounts = {
    bphenriques = { gid = 5000; };
    media = { gid = 5001; };
    shared = { gid = 5002; };
  };
  custom.homelab.ingress = {
    allowedInterfaces = [ "bond0" "wg0" ];
    acme = {
      dnsProvider = "cloudflare";
      email = private.settings.cloudflare.email;
      credentialsEnvFile = config.sops.templates."traefik-cloudflare".path;
    };
  };

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
  sops = {
    defaultSopsFile = private.sopsSecretsFile;
    age.keyFile = "/var/lib/sops-nix/system-keys.txt";
    secrets.cloudflare_dns_api_token = { };
    templates."traefik-cloudflare" = {
      owner = "traefik";
      content = ''
        CF_DNS_API_TOKEN=${config.sops.placeholder.cloudflare_dns_api_token}
        CF_API_EMAIL=${private.settings.cloudflare.email}
      '';
    };
  };

  # Users
  users.mutableUsers = false;
  nix.settings.trusted-users = [ config.users.users.bphenriques.name ];

  system.stateVersion = "25.11"; # The release version of the first install of this system!
}
