{ config, lib, inputs, private, ... }:
let
  # Ad-hoc (non-private) users; per-user service config rides under `services` on selfhost.users.
  extraUsers = {
    home = {
      email = "home@localhost";
      firstName = "Home";
      lastName = "User";
      groups = [ config.selfhost.groups.users ];
      auth.oidc.enable = false; # ad-hoc user, no OIDC account
      services = {
        jellyfin = {
          enable = true;
          passwordFile = config.selfhost.runtimeSecrets.home-jellyfin-initial-credentials.path;
        };
        seerr = {
          enable = true;
          permissions = {
            autoApprove = true;
            advancedRequests = true;
            viewRecentlyAdded = true;
          };
        };
      };
    };

    guest = {
      email = "guest@localhost";
      firstName = "Guest";
      lastName = "User";
      groups = [ config.selfhost.groups.guests ];
      auth.oidc.enable = false;
      services = {
        jellyfin = {
          enable = true;
          passwordFile = config.selfhost.runtimeSecrets.guest-jellyfin-initial-credentials.path;
        };
        kavita = {
          enable = true;
          passwordFile = config.selfhost.runtimeSecrets.guest-kavita-initial-credentials.path;
        };
        seerr = {
          enable = true;
          permissions = {
            autoApprove = false;
            advancedRequests = false;
            viewRecentlyAdded = true;
          };
        };
      };
    };
  };
in
{
  imports = [
    inputs.selfhost-nix.nixosModules.default
    ./datastores
    ./services
    ./tasks
    ./monitoring
    ../../../profiles/nixos/selfhost-smb-client.nix
    ../../../profiles/nixos/backup-backblaze.nix
  ];

  fleet = {
    locale = {
      latitude = 38.736946;
      longitude = -9.142685;
    };
  };

  # Landing-page opt-in for framework services; category comes from each service's meta.category.
  selfhost.services = {
    pocket-id.extraConfig.landingPage = { enable = true; order = 0; };
    tinyauth.extraConfig.landingPage.enable = true;
    gitea.extraConfig.landingPage.enable = true;
    bentopdf.extraConfig.landingPage.enable = true;
    miniflux.extraConfig.landingPage.enable = true;
    radicale.extraConfig.landingPage.enable = true;
    ntfy.extraConfig.landingPage.enable = true;
    prometheus.extraConfig.landingPage.enable = true;
    grafana.extraConfig.landingPage.enable = true;
    alertmanager.extraConfig.landingPage.enable = true;
  };

  selfhost = {
    groups = { inherit (private.groups) admin users; };
    apps = {
      radicale.enable = true;
      miniflux.enable = true;
      bentopdf.enable = true;
      gitea = {
        enable = true;
        ssh = {
          enable = true;
          inherit (config.fleet.forge.sshEndpoint) port;
        };
      };
    };

    auth = {
      forwardAuth.tinyauth.enable = true;
      oidc = {
        pocket-id.enable = true;
        rotation.enable = true; # weekly @ 03:00
      };
    };

    tasks.backup.integrations.notify = {
      topic = "backup";
      failureTopic = "admin";
    };

    notify = {
      ntfy.enable = true;
      ntfy.remotePublishers.storage-backup.topics = [ "backup" "admin" ];
      defaultTopic = "admin";
      topics = lib.mapAttrs (_: public: { inherit public; }) config.fleet.notifyTopics;
    };

    mail = private.settings.smtp // {
      passwordFile = config.sops.secrets."smtp-password".path;
    };

    storage.mounts.smb.shares = {
      bphenriques = { gid = 5000; };
      media = { gid = 5001; };
      shared = { gid = 5002; };
      # Only the two growing subfolders live on the NAS (thumbs stays local). uid too: the EXDEV fallback ends in owner-only utimes().
      immich-uploads = {
        localMount = "/var/lib/immich/library";
        uid = config.users.users.immich.uid;
        gid = 5004;
      };
      immich-encoded-video = {
        localMount = "/var/lib/immich/encoded-video";
        uid = config.users.users.immich.uid;
        gid = 5005;
      };
    };

    ingress = {
      inherit (private.settings) domain;
      allowedInterfaces = [ "bond0" "wg0" ];
      traefik.enable = true;
      acme = {
        email = private.settings.cloudflare.email;
        dns01 = {
          provider = "cloudflare";
          credentialsEnvFile = config.sops.templates."traefik-cloudflare".path;
        };
      };
    };

    monitoring = {
      enable = true;
      alertmanager.enable = true;
      retentionTime = "365d";
      retentionSize = "5GB";   # ~15 targets × ~200 metrics × 60s ≈ 4.7 GB/year; size is the effective bound
      scrapeInterval = "60s";
    };

    users = private.users // extraUsers;

    # agent-alfredo-vm's forge identity: the key is generated in the guest, so only the public half is declared here.
    serviceAccounts.personal-agent = {
      description = "agent-alfredo-vm's forge principal, for git over SSH to its inbox repository";
      services.gitea = {
        enable = true;
        sshKeys = [
          { key = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOHDIQhIOmKJMbCGKncBgpYnKnReeUR4TK1CgxMZbbj+ personal-agent@agent-vm"; }
        ];
      };
    };
  };

  # Upstream's 12h would expire a backup failure before a phone switched off for the weekend reads it.
  services.ntfy-sh.settings.cache-duration = "72h";

  virtualisation = {
    podman.enable = true;
    podman.autoPrune = {
      enable = true;
      dates = "weekly";
      flags = [ "--all" ]; # image tags pile up on every bump; only running containers keep theirs
    };
    oci-containers.backend = "podman";
    containers.containersConf.settings.containers = {
      default_capabilities = [];
      pids_limit = 100;
    };
  };

  # Firewall
  networking.firewall.interfaces = {
    bond0.allowedTCPPorts = [ config.selfhost.apps.gitea.ssh.port ];
    podman0.allowedTCPPorts = [ 443 ]; # Allow containers to reach Traefik for OIDC discovery/token exchange
  };

  # Secrets
  sops = {
    secrets.cloudflare_dns_api_token = { };
    templates."traefik-cloudflare" = {
      owner = "traefik";
      content = ''
        CF_DNS_API_TOKEN=${config.sops.placeholder.cloudflare_dns_api_token}
        CF_API_EMAIL=${private.settings.cloudflare.email}
      '';
    };
  };

  # Owned by Pocket-ID's user, the only SMTP consumer; a second one would need its own copy.
  sops.secrets."smtp-password".owner = config.services.pocket-id.user;

  # Bootstrap-only creds, never re-read, so a lost file may regenerate instead of failing activation.
  selfhost.runtimeSecrets = {
    home-jellyfin-initial-credentials.restartUnits = [ "jellyfin-configure.service" ];
    guest-jellyfin-initial-credentials.restartUnits = [ "jellyfin-configure.service" ];
    guest-kavita-initial-credentials.restartUnits = [ "kavita-configure.service" ];
  };

  systemd.services.sshd.serviceConfig.Slice = "critical.slice";
}
