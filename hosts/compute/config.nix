{ config, pkgs, lib, self, ... }:
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
  # Invert { hostname = ip; } to { ip = [hostnames]; } for /etc/hosts
  networking.hosts = lib.foldlAttrs (acc: name: ip: acc // { ${ip} = (acc.${ip} or []) ++ [ name ]; }) {} self.shared.networks.main.hosts;
  custom.homelab.paths = {
    media.root = config.custom.homelab.smb.mounts.media.localMount;
    users.bphenriques.root = config.custom.homelab.smb.mounts.bphenriques.localMount;
  };
  custom.homelab.smb = {
    enable = true;
    hostname = self.shared.networks.main.hosts.bruno-home-nas;
    credentialsPath = config.sops.templates."homelab-samba-credentials".path;
    mounts = {
      bphenriques = { gid = 5000; };
      media = { gid = 5001; };
      shared = { gid = 5002; };
    };
  };
  sops = {
    secrets."homelab/samba/username" = { };
    secrets."homelab/samba/password" = { };
    secrets.cloudflare_dns_api_token = { };
    templates."homelab-samba-credentials" = {
      owner = "root";
      group = "root";
      mode = "0400";
      content = ''
        username=${config.sops.placeholder."homelab/samba/username"}
        password=${config.sops.placeholder."homelab/samba/password"}
      '';
    };
    templates."traefik-cloudflare" = {
      owner = "traefik";
      content = ''
        CF_DNS_API_TOKEN=${config.sops.placeholder.cloudflare_dns_api_token}
      '';
    };
  };
  custom.homelab.ingress = {
    allowedInterfaces = [ "bond0" "wg0" ];
    cloudflareTokenEnvFile = config.sops.templates."traefik-cloudflare".path;
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

  # FIXME: Temporary
  environment.systemPackages = [
    pkgs.powertop
    pkgs.usbutils
  ];

  # Secrets
  sops.defaultSopsFile = self.private.hosts.compute.sopsSecretsFile;
  sops.age.keyFile = "/var/lib/sops-nix/system-keys.txt";

  # Users
  users.mutableUsers = false;
  nix.settings.trusted-users = [ config.users.users.bphenriques.name ];

  system.stateVersion = "25.11"; # The release version of the first install of this system!
}
