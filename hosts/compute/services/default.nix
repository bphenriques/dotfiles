{ self, ...}: {
  imports = [
    ./pocket-id
    ./tinyauth
    ./miniflux

    ./jellyseerr
    ./tandoor-recipes
    #./obsidian-livesync

    #./immich
    #./transmission

    # Servarr
    ./prowlarr
    ./sonarr
    ./radarr
    ./recyclarr.nix
  ];

  custom.home-server = {
    enable = true;
    domain = self.settings.compute.domain;
    cloudflareEmail = self.settings.cloudflareEmail;

    # Media quality profiles (synced by recyclarr from TRaSH guides)
    media = {
      radarr = {
        qualityDefinitionTemplate = "radarr-quality-definition-movie";
        defaultProfile = "hd";
        profiles = {
          hd = {
            name = "HD Bluray + WEB";
            recyclarrTemplates = [
              "radarr-quality-profile-hd-bluray-web"
              "radarr-custom-formats-hd-bluray-web"
            ];
          };
          uhd = {
            name = "UHD Bluray + WEB";
            recyclarrTemplates = [
              "radarr-quality-profile-uhd-bluray-web"
              "radarr-custom-formats-uhd-bluray-web"
            ];
          };
        };
      };

      sonarr = {
        qualityDefinitionTemplate = "sonarr-quality-definition-series";
        defaultProfile = "hd";
        profiles = {
          hd = {
            name = "WEB-1080p";
            recyclarrTemplates = [
              "sonarr-quality-profile-web-1080p-v4"
              "sonarr-custom-formats-web-1080p-v4"
            ];
          };
          uhd = {
            name = "WEB-2160p";
            recyclarrTemplates = [
              "sonarr-quality-profile-web-2160p-v4"
              "sonarr-custom-formats-web-2160p-v4"
            ];
          };
        };
      };
    };
  };
}
