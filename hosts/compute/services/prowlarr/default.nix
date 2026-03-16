{ config, pkgs, lib, ... }:
let
  serviceCfg = config.custom.homelab.services.prowlarr;
  homelabMounts = config.custom.homelab.smb.mounts;
in
{
  imports = [ ./configure.nix ];

  custom.homelab.services.prowlarr = {
    description = "Manage *rr services";
    version = config.services.prowlarr.package.version;
    homepage = config.services.prowlarr.package.meta.homepage;
    category = "Media";
    port = 9096;
    secrets = {
      files.api-key = { rotatable = true; };
      templates."prowlarr.env".content = ''
        PROWLARR__AUTH__APIKEY=${serviceCfg.secrets.placeholder.api-key}
      '';
      systemd.dependentServices = [ "prowlarr" "prowlarr-configure" ];
    };
    healthcheck.path = "/ping";
    forwardAuth.enable = true;
    integrations.homepage.enable = true;
    integrations.ntfy.enable = true;
    integrations.ntfy.topic = "admin";
  };

  services.prowlarr = {
    enable = true;
    settings.server.port = serviceCfg.port;
    environmentFiles = [ serviceCfg.secrets.templates."prowlarr.env".path ];
  };

  custom.homelab.smb.mounts.media.systemd.dependentServices = [ "prowlarr" ];
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
