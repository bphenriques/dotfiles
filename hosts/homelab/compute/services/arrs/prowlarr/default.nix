{ config, ... }:
let
  serviceCfg = config.custom.homelab.services.prowlarr;
  homelabMounts = config.custom.fileSystems.homelab.mounts;
in
{
  imports = [ ./configure.nix ];

  custom.homelab.services.prowlarr = {
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

  custom.fileSystems.homelab.mounts.media.systemd.dependentServices = [ "prowlarr" ];
  systemd.services.prowlarr = {
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
