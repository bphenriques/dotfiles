{ self, config, ...}:
let
  hosts = self.shared.networks.main.hosts;
in
{
  imports = [
    ./radarr
    ./sonarr
    ./immich
    ./jellyfin
    ./kavita
    ./miniflux
    ./pocket-id
    ./jellyseerr
    ./prowlarr
    ./cleanuparr.nix
    ./romm
    ./homepage
    ./syncthing.nix
    ./tinyauth.nix
    ./transmission.nix
    ./ntfy
    ./wireguard
    ./home-assistant.nix
    ./radicale
    ./tandoor-recipes
    ./filebrowser
    ./gitea
    ./bentopdf.nix
    ./larapaper.nix
    ./grist
  ];

  custom.homelab = {
    enable = true;
    domain = self.private.hosts.compute.settings.domain;
    locale = {
      timezone = config.time.timeZone;
      language = "pt-PT";
      currency = "EUR";
      latitude = 38.736946;
      longitude = -9.142685;
    };
    ingress.cloudflareEmail = self.private.hosts.compute.settings.cloudflare.email;
    smtp = self.private.hosts.compute.settings.smtp // {
      passwordFile = config.sops.secrets."smtp-password".path;
    };

    external = {
      synology-dsm = {
        displayName = "Synology";
        description = "NAS";
        category = "Administration";
        url = "http://${hosts.bruno-home-nas}:5000";
      };
      inky = {
        displayName = "Inky";
        description = "E-Ink Display";
        category = "Media";
        url = "http://${hosts.inky}:5000";
        tab = "Admin";
      };
      jetkvm = {
        displayName = "JetKVM";
        description = "Remote KVM";
        category = "Administration";
        url = "http://${hosts.jetkvm}";
      };
    };

    # Individual users whose information is kept private
    users = self.private.hosts.compute.settings.users // {
      home = {
        email = "home@localhost";
        firstName = "Home";
        lastName = "User";
        groups = [ config.custom.homelab.groups.users ];
        services = {
          oidc.enable = false; # ad-hoc user, no OIDC account
          jellyfin = {
            enable = true;
            passwordFile = config.sops.secrets."jellyfin/home/password".path;
          };
          jellyseerr = {
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
        groups = [ config.custom.homelab.groups.guests ];
        services = {
          oidc.enable = false;
          jellyfin = {
            enable = true;
            passwordFile = config.sops.secrets."guest/password".path;
          };
          kavita = {
            enable = true;
            passwordFile = config.sops.secrets."guest/password".path;
          };
          jellyseerr = {
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

  sops.secrets."smtp-password" = { };
  sops.secrets."jellyfin/home/password" = { };
  sops.secrets."guest/password" = { };
}
