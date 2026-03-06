{ self, config, ...}: {
  imports = [
    ./arrs
    ./immich
    ./jellyfin
    ./kavita
    ./miniflux
    ./pocket-id
    ./romm.nix
    ./homepage.nix
    ./obsidian-livesync.nix
    ./syncthing.nix
    ./tinyauth.nix
    ./transmission.nix
    ./wireguard
    # TODO: collabora-online
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
         pocket-id.enable = false; # ad-hoc
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
