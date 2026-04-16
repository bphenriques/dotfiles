{ lib, ... }:
let
  # Quality profile definition (v8: guide-backed via trash_id)
  qualityProfileOpt = lib.types.submodule {
    options = {
      name = lib.mkOption {
        type = lib.types.str;
        description = "Name of the quality profile (used for adoption and display)";
      };
      trashId = lib.mkOption {
        type = lib.types.str;
        description = "TRaSH guide-backed quality profile trash_id";
      };
    };
  };

  # Profiles must always define a `default` entry.
  profilesOpt = lib.types.submodule {
    freeformType = lib.types.attrsOf qualityProfileOpt;
    options.default = lib.mkOption {
      type = qualityProfileOpt;
      description = "Default quality profile used by service setup";
    };
  };

  # Service-specific settings (radarr, sonarr)
  mediaServiceOpt = lib.types.submodule {
    options = {
      qualityDefinitionType = lib.mkOption {
        type = lib.types.str;
        description = "Recyclarr quality_definition type (e.g., 'movie' or 'series')";
      };
      profiles = lib.mkOption {
        type = profilesOpt;
        description = "Available quality profiles";
      };
    };
  };
in
{
  options.custom.homelab.media = {
    jellyfin.serverId = lib.mkOption {
      type = lib.types.str;
      description = "Jellyfin server ID for integrations (e.g., Seerr)";
    };

    radarr = lib.mkOption {
      type = mediaServiceOpt;
      description = "Radarr quality profile settings";
    };

    sonarr = lib.mkOption {
      type = mediaServiceOpt;
      description = "Sonarr quality profile settings";
    };
  };
}
