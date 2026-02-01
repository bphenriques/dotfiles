{ config, ... }:
let
  homelabCfg = config.custom.fileSystems.homelab;
in
{
  imports = [ ./setup.nix ];

  custom.home-server.routes.jellyfin.port = 8096;
  custom.home-server.oidc.clients.jellyfin = {
    callbackURLs = [
      "${config.custom.home-server.routes.jellyfin.publicUrl}/sso/OID/redirect/PocketID"
    ];
  };

  services.jellyfin.enable = true;
  users.users.jellyfin.extraGroups = [ homelabCfg.mounts.media.group ]; # Ensure jellyfin has access to the media
}
