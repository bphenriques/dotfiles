{ config, ... }:
let
  serviceCfg = config.custom.home-server.services.jellyfin;
in
{
  imports = [ ./post-start.nix ];

  custom.home-server.services.jellyfin = {
    port = 8096;
    dashboard = {
      enable = true;
      category = "Media";
      description = "Media Player";
      icon = "jellyfin.svg";
    };
  };

  custom.home-server.oidc.clients.jellyfin = {
    callbackURLs = [ "${serviceCfg.publicUrl}/sso/OID/redirect/PocketID" ];
  };

  services.jellyfin.enable = true;
  users.users.jellyfin.extraGroups = [ config.custom.fileSystems.homelab.mounts.media.group ];
}
