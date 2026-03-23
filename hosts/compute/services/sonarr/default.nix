_: {
  imports = [
    (import ../lib/mkArrService.nix {
      name = "sonarr";
      port = 9097;
      description = "TV Tracker";
      rootPath = config: config.custom.homelab.paths.media.tv;
      categoryField = "tvCategory";
      forwardAuthGroup = config: config.custom.homelab.groups.admin;
    })
    ./backup.nix
  ];

  custom.homelab.media.sonarr = {
    qualityDefinitionTemplate = "sonarr-quality-definition-series";
    profiles = {
      default = {
        name = "WEB-1080p";
        recyclarrTemplates = [
          "sonarr-quality-definition-series"
          "sonarr-v4-quality-profile-web-1080p"
          "sonarr-v4-custom-formats-web-1080p"
        ];
      };
      uhd = {
        name = "WEB-2160p";
        recyclarrTemplates = [
          "sonarr-quality-definition-series"
          "sonarr-v4-quality-profile-web-2160p"
          "sonarr-v4-custom-formats-web-2160p"
        ];
      };
    };
  };
}
