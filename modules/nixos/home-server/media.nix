{ lib, config, ... }:
let
  cfg = config.custom.home-server.media;

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
  options.custom.home-server.media = {
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
        assertion = cfg.radarr.profiles ? ${cfg.radarr.defaultProfile};
        message = "Radarr defaultProfile '${cfg.radarr.defaultProfile}' must exist in profiles";
      }
      {
        assertion = cfg.sonarr.profiles ? ${cfg.sonarr.defaultProfile};
        message = "Sonarr defaultProfile '${cfg.sonarr.defaultProfile}' must exist in profiles";
      }
    ];
  };
}
