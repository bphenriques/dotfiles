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
    ./romm.nix
    ./homepage
    ./obsidian-livesync
    ./syncthing.nix
    ./tinyauth.nix
    ./transmission.nix
    ./ntfy
    ./wireguard
    ./opencloud.nix
    ./home-assistant.nix
    ./radicale
    ./tandoor-recipes
  ];

  custom.homelab = {
    enable = true;
    domain = self.private.settings.hosts.compute.domain;
    ingress.cloudflareEmail = self.private.settings.cloudflare.email;

    external = {
      synology = {
        description = "NAS";
        category = "Administration";
        url = "http://${hosts.bruno-home-nas}:5000";
        icon = "synology-dsm.svg";
      };
      inky = {
        description = "E-Ink Display";
        category = "Media";
        url = "http://${hosts.inky}:5000";
        tab = "Admin";
      };
      jetkvm = {
        description = "Remote KVM";
        category = "Administration";
        url = "http://${hosts.jetkvm}";
      };
    };

    # Individual users whose information is kept private
    users = self.private.settings.users // {
      home = {
        email = "home@localhost";
        firstName = "Home";
        lastName = "User";
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
    };
  };

  sops.secrets."jellyfin/home/password" = { };
}
