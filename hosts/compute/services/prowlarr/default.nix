{ config, ... }:
let
  homelabMounts = config.custom.fileSystems.homelab.mounts;
in
{
  imports = [ ./setup.nix ];

  custom.home-server.routes.prowlarr = {
    port = 9096;
    forwardAuth.enable = true;
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
    serviceConfig = {
      SupplementaryGroups = [ homelabMounts.media.group ];
      Restart = "on-failure";
      RestartSec = "10s";
      RestartMaxDelaySec = "5min";
      RestartSteps = 5;
    };
    environment = {
      PROWLARR__AUTH__METHOD = "External";
      PROWLARR__LOG__LEVEL = "info";
    };
  };
}
