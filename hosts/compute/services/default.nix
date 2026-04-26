{ config, private, ... }:
let
  inherit (config.custom.fleet.lan) hosts;
in
{
  imports = [
    ./radarr
    ./sonarr
    ./immich
    ./jellyfin
    ./kapowarr.nix
    ./kavita
    ./miniflux
    ./pocket-id
    ./seerr
    ./prowlarr
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
    ./papra.nix
    ./bentopdf.nix
    ./grist
    ./mympd.nix
  ];

  custom.homelab = {
    enable = true;
    inherit (private.settings) domain;
    locale = {
      timezone = config.time.timeZone;
      language = "pt-PT";
      currency = "EUR";
      latitude = 38.736946;
      longitude = -9.142685;
    };
    ingress.cloudflareEmail = private.settings.cloudflare.email;
    smtp = private.settings.smtp // {
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
        url = "http://${hosts.inky}";
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
    users = private.settings.users // {
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

  sops.secrets."smtp-password" = { };
  sops.secrets."jellyfin/home/password" = { };
  sops.secrets."guest/password" = { };
}
