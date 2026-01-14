{ self, ...}: {
  imports = [
    ./pocket-id.nix
    ./miniflux.nix

    ./jellyseerr.nix
    ./transmission.nix
    ./tandoor-recipes.nix

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
