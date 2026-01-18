{ self, ...}: {
  imports = [
    ./pocket-id
    ./miniflux.nix

    ./jellyseerr.nix
    ./transmission.nix
    ./tandoor-recipes.nix
    #./obsidian-livesync

    #./immich

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
