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
        options.services.jellyfin = {
          enable = lib.mkEnableOption "Jellyfin account for this user";
          # FIXME: Remove once Seerr supports OIDC - used for local Jellyfin auth
          passwordFile = lib.mkOption {
            type = lib.types.nullOr lib.types.str;
            default = null;
            description = "Path to file containing local Jellyfin password (for Seerr auth until OIDC is supported)";
          };
        };
      }
    );
  };

  config = {
    selfhost = {
      services.jellyfin = {
        displayName = "Jellyfin";
        meta.homepage = "https://jellyfin.org";
        meta.description = "Media Player";
        meta.category = "media";
        port = 8096;
        access.allowedGroups = with config.selfhost.groups; [ guests users admin ];
        oidc = {
          enable = true;
          callbackURLs = [ "${serviceCfg.publicUrl}/sso/OID/redirect/PocketID" ];
          systemd.dependentServices = [ "jellyfin-configure" "jellyfin-sso-configure" ];
        };
        healthcheck.path = "/health";
        storage.smb = [ "media" ];
        extraConfig.landingPage.enable = true;
      };

      runtimeSecrets.jellyfin-admin-password = {
        regenerateIfMissing = false;
        restartUnits = [ "jellyfin-configure.service" "jellyfin-sso-configure.service" ];
      };
    };

    services.jellyfin.enable = true;
    users.users.jellyfin.extraGroups = [ selfhostMounts.media.group "video" "render" ];
    systemd.services.jellyfin = {
      environment.LIBVA_DRIVER_NAME = "iHD"; # Force iHD (intel-media-driver) over legacy i965
      serviceConfig.ReadOnlyPaths = [ pathsCfg.media.music.library ];
      serviceConfig.Slice = "throttled.slice";
    };

    assertions = [
      {
        assertion = selfhostMounts ? media;
        message = "Jellyfin requires selfhost.storage.smb.mounts.media to be configured.";
      }
    ];
  };
}
