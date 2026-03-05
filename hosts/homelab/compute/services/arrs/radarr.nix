{ config, pkgs, lib, self, ... }:
let
  radarr = import ./lib/mkArrService.nix { inherit config pkgs lib self; } {
    name = "radarr";
    port = 9098;
    description = "Movie Tracker";
    rootPath = config.custom.paths.media.movies;
    categoryField = "movieCategory";
    forwardAuthGroup = config.custom.homelab.groups.admin;
  };
  # TODO : https://gethomepage.dev/widgets/services/radarr/
  profiles = {
    custom.homelab.media.radarr = {
      qualityDefinitionTemplate = "radarr-quality-definition-movie";
      defaultProfile = "hd";

      # Recyclarr configuration using TRaSH Guide include templates
      # See: https://recyclarr.dev/reference/configuration/include/
      # See: https://trash-guides.info/
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
  };
in lib.foldl' lib.recursiveUpdate { } [ radarr profiles ]
