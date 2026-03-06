{ lib, config, ... }:
let
  cfg = config.custom.homelab.media;

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

  # Service-specific settings (radarr, sonarr)
  mediaServiceOpt = lib.types.submodule {
    options = {
      qualityDefinitionTemplate = lib.mkOption {
        type = lib.types.str;
        description = "Recyclarr quality definition template";
      };
      profiles = lib.mkOption {
        type = lib.types.attrsOf qualityProfileOpt;
        description = "Available quality profiles";
      };
      defaultProfile = lib.mkOption {
        type = lib.types.str;
        description = "Default quality profile name (must match a profile in profiles)";
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

  config = {
    assertions = [
      {
        assertion = lib.hasAttr cfg.radarr.defaultProfile cfg.radarr.profiles;
        message = "Radarr defaultProfile '${cfg.radarr.defaultProfile}' must exist in profiles";
      }
      {
        assertion = lib.hasAttr cfg.sonarr.defaultProfile cfg.sonarr.profiles;
        message = "Sonarr defaultProfile '${cfg.sonarr.defaultProfile}' must exist in profiles";
      }
    ];
  };
}
