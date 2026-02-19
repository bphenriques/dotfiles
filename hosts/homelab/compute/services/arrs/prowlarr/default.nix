{ config, ... }:
let
  serviceCfg = config.custom.home-server.services.prowlarr;
  homelabMounts = config.custom.fileSystems.homelab.mounts;
in
{
  imports = [ ./post-start.nix ];

  custom.home-server.services.prowlarr = {
    port = 9096;
    forwardAuth.enable = true;
    dashboard = {
      enable = true;
      category = "Admin";
      description = "Manage *rr services";
      icon = "prowlarr.svg";
    };
  };

  sops.secrets."prowlarr/api-key" = { };
  sops.templates."prowlarr.env".content = ''
    PROWLARR__AUTH__APIKEY=${config.sops.placeholder."prowlarr/api-key"}
  '';

  services.prowlarr = {
    enable = true;
    settings.server.port = serviceCfg.port;
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
