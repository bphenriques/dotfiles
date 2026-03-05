{ config, ... }:
let
  serviceCfg = config.custom.homelab.services.jellyfin;
  homelabMounts = config.custom.fileSystems.homelab.mounts;
in
{
  imports = [
    ./plugins.nix
    ./configure.nix
  ];

  custom.homelab.services.jellyfin = {
    port = 8096;
    secrets = {
      files.admin-password = { rotatable = false; };
      systemd.dependentServices = [ "jellyfin-configure" "jellyfin-sso-configure" ];
    };
    # TODO: https://gethomepage.dev/widgets/services/jellyfin/
    oidc = {
      enable = true;
      callbackURLs = [ "${serviceCfg.publicUrl}/sso/OID/redirect/PocketID" ];
      systemd.dependentServices = [ "jellyfin-configure" "jellyfin-sso-configure" ];
    };
    integrations.homepage = {
      enable = true;
      category = "Media";
      description = "Media Player";
    };
  };

  services.jellyfin.enable = true;
  users.users.jellyfin.extraGroups = [ homelabMounts.media.group "video" "render" ];
  custom.fileSystems.homelab.mounts.media.systemd.dependentServices = [ "jellyfin" ];

  assertions = [
    {
      assertion = homelabMounts ? media;
      message = "Jellyfin requires custom.fileSystems.homelab.mounts.media to be configured.";
    }
  ];
}
