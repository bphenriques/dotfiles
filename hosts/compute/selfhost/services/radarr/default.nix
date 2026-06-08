_: {
  imports = [
    (import ../lib/mkArrService.nix {
      name = "radarr";
      port = 9098;
      description = "Movie Tracker";
      rootPath = config: config.selfhost.paths.media.movies;
      categoryField = "movieCategory";
      forwardAuthGroup = config: config.selfhost.groups.admin;
    })
    ./backup.nix
  ];

  # Recyclarr v8 configuration using guide-backed quality profiles
  # See: https://recyclarr.dev/guide/upgrade-guide/v8.0/
  # See: https://trash-guides.info/
  selfhost.media.radarr = {
    qualityDefinitionType = "movie";
    profiles = {
      default = {
        name = "HD Bluray + WEB";
        trashId = "d1d67249d3890e49bc12e275d989a7e9";
      };
      uhd = {
        name = "UHD Bluray + WEB";
        trashId = "64fb5f9858489bdac2af690e27c8f42f";
      };
    };
  };
}
