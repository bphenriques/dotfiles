{ self, ...}: {
  imports = [
    ./pocket-id
    ./miniflux

    #./jellyseerr.nix
    #./transmission.nix
    #./tandoor-recipes.nix
    #./obsidian-livesync

    #./immich
    #./transmission

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
