{ config, pkgs, lib, self, ... }:
let
  sonarr = import ./lib/mkArrService.nix { inherit config pkgs lib self; } {
    name = "sonarr";
    port = 9097;
    description = "TV Tracker";
    rootPath = config.custom.homelab.paths.media.tv;
    categoryField = "tvCategory";
    forwardAuthGroup = config.custom.homelab.groups.admin;
  };
  profiles = {
    custom.homelab.media.sonarr = {
      qualityDefinitionTemplate = "sonarr-quality-definition-series";
      defaultProfile = "hd";
      profiles = {
        hd = {
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
  };
in lib.foldl' lib.recursiveUpdate { } [ sonarr profiles ]
