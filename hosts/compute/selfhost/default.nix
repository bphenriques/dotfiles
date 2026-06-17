{ config, pkgs, lib, self, private, ... }:
{
  imports = [
    ./datastores
    ./services
    ./tasks
    ./monitoring
    ../../../profiles/nixos/selfhost-smb-client.nix
  ];

  custom.fleet = import ../../shared.nix;
  custom.locale = {
    timezone = config.time.timeZone;
    language = "pt-PT";
    currency = "EUR";
    latitude = 38.736946;
    longitude = -9.142685;
  };

  selfhost = {
    enable = true;
    inherit (private.settings) domain;

    auth.oidc.pocket-id.enable = true;
    auth.forwardAuth.tinyauth.enable = true;

    notify.ntfy.enable = true;
    notify.topics = {
      media.public = true;
      download.public = false;
      admin.public = false;
    };

    mail = private.settings.smtp // {
      passwordFile = config.sops.secrets."smtp-password".path;
    };

    storage.smb.mounts = {
      bphenriques = { gid = 5000; };
      media = { gid = 5001; };
      shared = { gid = 5002; };
    };

    ingress = {
      allowedInterfaces = [ "bond0" "wg0" ];
      traefik.enable = true;
      acme = {
        dnsProvider = "cloudflare";
        email = private.settings.cloudflare.email;
        credentialsEnvFile = config.sops.templates."traefik-cloudflare".path;
      };
    };

    monitoring = {
      enable = true;
      alertmanager.enable = true;
      retentionTime = "365d";
      retentionSize = "5GB";   # ~15 targets × ~200 metrics × 60s ≈ 4.7 GB/year; size is the effective bound
      scrapeInterval = "60s";
    };

    resourceControl.slices = {
      throttled.sliceConfig = {
        AllowedCPUs = "1-2";  # cores 0,3 reserved for system/critical (core 0 handles timer/boot interrupts)
        CPUQuota = "150%";    # hard cap prevents turbo heat-soak on passively-cooled N150
        CPUWeight = 20;
        MemoryHigh = "16G";
        MemoryMax = "20G";
      };
      critical = {
        extraSystemdServices = [ "sshd" "dhcpcd" ];
        sliceConfig.CPUWeight = 1000;
      };
    };

    users = private.settings.users // {
      home = {
        email = "home@localhost";
        firstName = "Home";
        lastName = "User";
        groups = [ config.selfhost.groups.users ];
        services = {
          oidc.enable = false; # ad-hoc user, no OIDC account
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
        services = {
          oidc.enable = false;
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
          filebrowser = {
            enable = true;
            permissions = { create = true; delete = false; rename = false; modify = false; execute = false; share = false; download = true; };
          };
        };
      };
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

  # FIXME: Pocket-ID (the only SMTP consumer) reads mail.passwordFile directly, so own the secret by its
  # service user to make it readable. A second consumer would need a per-service owner-adjusted copy.
  sops.secrets."smtp-password".owner = config.services.pocket-id.user;

  # Bootstrap-only creds: set at account creation, then changed in-app. Never re-read, so unlike the admin
  # secret these keep regenerateIfMissing's default (true) — a lost file regenerates instead of failing.
  selfhost.runtimeSecrets = {
    home-jellyfin-initial-credentials.restartUnits = [ "jellyfin-configure.service" ];
    guest-jellyfin-initial-credentials.restartUnits = [ "jellyfin-configure.service" ];
    guest-kavita-initial-credentials.restartUnits = [ "kavita-configure.service" ];
  };
}
