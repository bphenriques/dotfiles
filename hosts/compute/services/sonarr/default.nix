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

  # Recyclarr v8 configuration using guide-backed quality profiles
  # See: https://recyclarr.dev/guide/upgrade-guide/v8.0/
  custom.homelab.media.sonarr = {
    qualityDefinitionType = "series";
    profiles = {
      default = {
        name = "WEB-1080p";
        trashId = "72dae194fc92bf828f32cde7744e51a1";
      };
      uhd = {
        name = "WEB-2160p";
        trashId = "d1498e7d189fbe6c7110ceaabb7473e6";
      };
    };
  };
}
