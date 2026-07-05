{ config, lib, ... }:
let
  serviceCfg = config.selfhost.services.jellyfin;
  pathsCfg = config.custom.paths;
  selfhostMounts = config.selfhost.storage.smb.mounts;
in
{
  imports = [
    ./plugins.nix
    ./configure.nix
  ];

  options.selfhost.users = lib.mkOption {
    type = lib.types.attrsOf (
      lib.types.submodule {
        options.extraConfig = lib.mkOption {
          type = lib.types.submodule {
            options.services.jellyfin = {
              enable = lib.mkEnableOption "Jellyfin account for this user";
              # FIXME: Remove once Seerr supports OIDC - used for local Jellyfin auth
              passwordFile = lib.mkOption {
                type = lib.types.nullOr lib.types.str;
                default = null;
                description = "Path to file containing local Jellyfin password (for Seerr auth until OIDC is supported)";
              };
            };
          };
        };
      }
    );
  };

  config = {
    selfhost = {
      services.jellyfin = {
        displayName = "Jellyfin";
        description = "Media Player";
        port = 8096;
        access.allowedGroups = with config.selfhost.groups; [ guests users admin ];
        oidc = {
          enable = true;
          callbackURLs = [ "${serviceCfg.publicUrl}/sso/OID/redirect/PocketID" ];
          systemd.dependentServices = [ "jellyfin-configure" "jellyfin-sso-configure" ];
        };
        healthcheck.path = "/health";
        storage.smb = [ "media" ];
      };

      runtimeSecrets.jellyfin-admin-password = {
        regenerateIfMissing = false;
        restartUnits = [ "jellyfin-configure.service" "jellyfin-sso-configure.service" ];
      };
    };

    services.jellyfin.enable = true;
    users.users.jellyfin.extraGroups = [ selfhostMounts.media.group "video" "render" ];
    systemd.services.jellyfin.environment.LIBVA_DRIVER_NAME = "iHD"; # Force iHD (intel-media-driver) over legacy i965
    systemd.services.jellyfin.serviceConfig.ReadOnlyPaths = [ pathsCfg.media.music.library ];

    assertions = [
      {
        assertion = selfhostMounts ? media;
        message = "Jellyfin requires selfhost.storage.smb.mounts.media to be configured.";
      }
    ];
  };
}
