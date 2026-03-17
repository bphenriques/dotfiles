{ config, ... }:
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

  custom.homelab.services.jellyfin = {
    description = "Media Player";
    version = config.services.jellyfin.package.version;
    homepage = config.services.jellyfin.package.meta.homepage;
    category = "Media";
    port = 8096;
    secrets = {
      files.admin-password = { rotatable = false; };
      systemd.dependentServices = [ "jellyfin-configure" "jellyfin-sso-configure" ];
    };
    oidc = {
      enable = true;
      callbackURLs = [ "${serviceCfg.publicUrl}/sso/OID/redirect/PocketID" ];
      systemd.dependentServices = [ "jellyfin-configure" "jellyfin-sso-configure" ];
    };
    healthcheck.path = "/health";
    integrations.homepage.enable = true;
  };

  services.jellyfin.enable = true;
  users.users.jellyfin.extraGroups = [ homelabMounts.media.group "video" "render" ];
  systemd.services.jellyfin.environment.LIBVA_DRIVER_NAME = "iHD"; # Force iHD (intel-media-driver) over legacy i965
  custom.homelab.smb.mounts.media.systemd.dependentServices = [ "jellyfin" ];

  systemd.services.jellyfin.serviceConfig.BindReadOnlyPaths = [ pathsCfg.media.music.library ];

  assertions = [
    {
      assertion = homelabMounts ? media;
      message = "Jellyfin requires custom.homelab.smb.mounts.media to be configured.";
    }
  ];
}
