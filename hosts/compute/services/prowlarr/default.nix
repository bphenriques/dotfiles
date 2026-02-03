{ config, ... }:
let
  homelabMounts = config.custom.fileSystems.homelab.mounts;
in
{
  imports = [ ./setup.nix ];

  custom.home-server.routes.prowlarr = {
    port = 9096;
    requiresAuth = true;
  };

  sops.secrets."prowlarr/api-key" = { };
  sops.templates."prowlarr.env".content = ''
    PROWLARR__AUTH__APIKEY=${config.sops.placeholder."prowlarr/api-key"}
  '';

  services.prowlarr = {
    enable = true;
    settings.server.port = config.custom.home-server.routes.prowlarr.port;
    environmentFiles = [ config.sops.templates."prowlarr.env".path ];
  };

  systemd.services.prowlarr = {
    requires = [ homelabMounts.media.automountUnit ];
    after = [ homelabMounts.media.automountUnit ];
    serviceConfig.SupplementaryGroups = [ homelabMounts.media.group ];
    environment.PROWLARR__AUTH__METHOD = "External";
  };
}
