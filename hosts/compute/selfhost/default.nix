{ config, pkgs, lib, self, private, ... }:
let
  # Full per-user data; framework keys go to selfhost.users, the `custom` key to custom.users.
  rawUsers = private.settings.users // {
    home = {
      email = "home@localhost";
      firstName = "Home";
      lastName = "User";
      groups = [ config.selfhost.groups.users ];
      auth.oidc.enable = false; # ad-hoc user, no OIDC account
      custom = {
        services.jellyfin = {
          enable = true;
          passwordFile = config.selfhost.runtimeSecrets.home-jellyfin-initial-credentials.path;
        };
        services.seerr = {
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
      custom = {
        services.jellyfin = {
          enable = true;
          passwordFile = config.selfhost.runtimeSecrets.guest-jellyfin-initial-credentials.path;
        };
        services.kavita = {
          enable = true;
          passwordFile = config.selfhost.runtimeSecrets.guest-kavita-initial-credentials.path;
        };
        services.seerr = {
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
    ./datastores
    ./services
    ./tasks
    ./monitoring
    ../../../profiles/nixos/selfhost-smb-client.nix
  ];

  custom = {
    fleet = import ../../shared.nix;
    locale = {
      timezone = config.time.timeZone;
      language = "pt-PT";
      currency = "EUR";
      latitude = 38.736946;
      longitude = -9.142685;
    };

    # Consumer-owned per-user (not selfhost-nix); joined with selfhost.users by name in our configures.
    users = lib.mapAttrs (n: u:
      { inherit (config.selfhost.users.${n}) username email name isAdmin; } // (u.custom or { })
    ) rawUsers;
  };

  selfhost = {
    enable = true;
    inherit (private.settings) domain;

    # Curated apps that need no host-specific config beyond enabling them (per-user opt-ins live on
    # selfhost.users.<name>.apps.<name>); apps with deployment overrides keep their own ./services file.
    apps.radicale.enable = true;
    apps.miniflux.enable = true;
    apps.bentopdf.enable = true;

    auth = {
      forwardAuth.tinyauth.enable = true;
      oidc = {
        pocket-id.enable = true;
        rotation = {
          enable = true; # weekly @ 03:00; alert to the admin topic on failure
          notifyTopic = "admin";
        };
      };
    };

    notify = {
      ntfy.enable = true;
      topics = {
        media.public = true;
        download.public = false;
        admin.public = false;
      };
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

    # Framework view: drop the consumer `custom` key (it goes to custom.users above).
    users = lib.mapAttrs (_: u: builtins.removeAttrs u [ "custom" ]) rawUsers;
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

  # Bootstrap-only creds: set at account creation, then changed in-app. Never re-read, so a lost file may
  # regenerate (regenerateIfMissing's default, true) instead of failing the activation.
  selfhost.runtimeSecrets = {
    home-jellyfin-initial-credentials.restartUnits = [ "jellyfin-configure.service" ];
    guest-jellyfin-initial-credentials.restartUnits = [ "jellyfin-configure.service" ];
    guest-kavita-initial-credentials.restartUnits = [ "kavita-configure.service" ];
  };
}
