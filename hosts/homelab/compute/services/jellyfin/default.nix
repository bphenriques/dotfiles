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

  custom.homelab = {
    services.jellyfin = {
      port = 8096;
      dashboard = {
        enable = true;
        category = "Media";
        description = "Media Player";
        icon = "jellyfin.svg";
      };
    };
    oidc.clients.jellyfin = {
      callbackURLs = [ "${serviceCfg.publicUrl}/sso/OID/redirect/PocketID" ];
      systemd.dependentServices = [ "jellyfin-configure" "jellyfin-sso-configure" ];
    };
  };

  services.jellyfin.enable = true;
  users.users.jellyfin.extraGroups = [ homelabMounts.media.group ];
  custom.fileSystems.homelab.mounts.media.systemd.dependentServices = [ "jellyfin" ];

  assertions = [
    {
      assertion = homelabMounts ? media;
      message = "Jellyfin requires custom.fileSystems.homelab.mounts.media to be configured.";
    }
  ];
}
