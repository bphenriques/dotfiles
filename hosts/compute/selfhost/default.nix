{ config, pkgs, lib, self, inputs, private, ... }:
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
    ../../../profiles/nixos/capabilities/selfhost-smb-client.nix
  ];

  custom = {
    locale = {
      timezone = config.time.timeZone;
      latitude = 38.736946;
      longitude = -9.142685;
    };
  };

  # Landing-page opt-in for framework-provided services; their category comes from each service's
  # meta.category default. Pocket ID is pinned to the top (order 0).
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
    enable = true;
    inherit (private.settings) domain;

    # Curated apps that need no host-specific config beyond enabling them (per-user opt-ins live on
    # selfhost.users.<name>.apps.<name>); apps with deployment overrides keep their own ./services file.
    apps = {
      radicale.enable = true;
      miniflux.enable = true;
      bentopdf.enable = true;
      gitea = {
        enable = true;
        ssh.enable = true;
      };
    };

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

    users = private.settings.users // extraUsers;
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

  systemd.services.sshd.serviceConfig.Slice = "critical.slice";
}
