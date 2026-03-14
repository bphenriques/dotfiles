{ lib, ... }:
let
  # Quality profile definition
  qualityProfileOpt = lib.types.submodule {
    options = {
      name = lib.mkOption {
        type = lib.types.str;
        description = "Name of the quality profile as created by recyclarr";
      };
      recyclarrTemplates = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        description = "Recyclarr include templates for this profile";
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
      qualityDefinitionTemplate = lib.mkOption {
        type = lib.types.str;
        description = "Recyclarr quality definition template";
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
      description = "Jellyfin server ID for integrations (e.g., Jellyseerr)";
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
