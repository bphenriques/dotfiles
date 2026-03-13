{ ... }: {
  imports = [
    (import ../lib/mkArrService.nix {
      name = "radarr";
      port = 9098;
      description = "Movie Tracker";
      rootPath = config: config.custom.homelab.paths.media.movies;
      categoryField = "movieCategory";
      forwardAuthGroup = config: config.custom.homelab.groups.admin;
    })
    ./backup.nix
  ];

  # Recyclarr configuration using TRaSH Guide include templates
  # See: https://recyclarr.dev/reference/configuration/include/
  # See: https://trash-guides.info/
  custom.homelab.media.radarr = {
    qualityDefinitionTemplate = "radarr-quality-definition-movie";
    defaultProfile = "hd";
    profiles = {
      hd = {
        name = "HD Bluray + WEB";
        recyclarrTemplates = [
          "radarr-quality-definition-movie"
          "radarr-quality-profile-hd-bluray-web"
          "radarr-custom-formats-hd-bluray-web"
        ];
      };
      uhd = {
        name = "UHD Bluray + WEB";
        recyclarrTemplates = [
          "radarr-quality-definition-movie"
          "radarr-quality-profile-uhd-bluray-web"
          "radarr-custom-formats-uhd-bluray-web"
        ];
      };
    };
  };
}
