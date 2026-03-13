{ self, config, ...}: {
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
    ./homepage.nix
    ./obsidian-livesync.nix
    ./syncthing.nix
    ./tinyauth.nix
    ./transmission.nix
    ./ntfy
    ./wireguard
    ./opencloud.nix
    ./home-assistant.nix
  ];

  custom.homelab = {
    enable = true;
    domain = self.private.settings.hosts.compute.domain;
    ingress.cloudflareEmail = self.private.settings.cloudflare.email;

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
