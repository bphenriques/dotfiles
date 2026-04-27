{ config, lib, ... }:
let
  serviceCfg = config.custom.homelab.services.jellyfin;
  pathsCfg = config.custom.homelab.paths;
  homelabMounts = config.custom.homelab.smb.mounts;
in
{
  imports = [
    ./plugins.nix
    ./configure.nix
  ];

  options.custom.homelab.users = lib.mkOption {
    type = lib.types.attrsOf (lib.types.submodule {
      options.services.jellyfin = {
        enable = lib.mkEnableOption "Jellyfin account for this user";
        # FIXME: Remove once Seerr supports OIDC - used for local Jellyfin auth
        passwordFile = lib.mkOption {
          type = lib.types.nullOr lib.types.str;
          default = null;
          description = "Path to file containing local Jellyfin password (for Seerr auth until OIDC is supported)";
        };
      };
    });
  };

  config = {
    custom.homelab.services.jellyfin = {
      displayName = "Jellyfin";
      metadata.description = "Media Player";
      metadata.version = config.services.jellyfin.package.version;
      metadata.homepage = config.services.jellyfin.package.meta.homepage;
      metadata.category = "Media";
      port = 8096;
      secrets = {
        files.admin-password = { rotatable = false; };
        systemd.dependentServices = [ "jellyfin-configure" "jellyfin-sso-configure" ];
      };
      access.allowedGroups = with config.custom.homelab.groups; [ guests users admin ];
      oidc = {
        enable = true;
        callbackURLs = [ "${serviceCfg.publicUrl}/sso/OID/redirect/PocketID" ];
        systemd.dependentServices = [ "jellyfin-configure" "jellyfin-sso-configure" ];
      };
      healthcheck.path = "/health";
      integrations.homepage.enable = true;
      storage.smb = [ "media" ];
      resourceControl = {
        slice = "throttled";
        systemdServices = [ "jellyfin" ];
      };
    };

    services.jellyfin.enable = true;
    users.users.jellyfin.extraGroups = [ homelabMounts.media.group "video" "render" ];
    systemd.services.jellyfin.environment.LIBVA_DRIVER_NAME = "iHD"; # Force iHD (intel-media-driver) over legacy i965
    systemd.services.jellyfin.serviceConfig.ReadOnlyPaths = [ pathsCfg.media.music.library ];

    assertions = [
      {
        assertion = homelabMounts ? media;
        message = "Jellyfin requires custom.homelab.smb.mounts.media to be configured.";
      }
    ];
  };
}
