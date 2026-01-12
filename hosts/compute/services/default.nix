{ self, ...}: {
  imports = [
    ./pocket-id.nix
    ./miniflux.nix

    ./jellyseerr.nix

    # Servarr
    ./prowlarr.nix
    ./sonarr.nix
    ./radarr.nix
  ];

  custom.home-server = {
    enable = true;
    domain = self.settings.compute.domain;
    cloudflareEmail = self.settings.cloudflareEmail;
  };
}


# booklore
# cleanuparr
# homepage
# immich
# obsidian-livesync
# romm
# syncthing
# tandoor
# transmission