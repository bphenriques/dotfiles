{ config, pkgs, lib, ... }:
let
  serviceCfg = config.custom.homelab.services.prowlarr;
  homelabMounts = config.custom.homelab.cifs.mounts;
in
{
  imports = [ ./configure.nix ];

  custom.homelab.services.prowlarr = {
    port = 9096;
    secrets = {
      files.api-key = { rotatable = true; };
      templates."prowlarr.env".content = ''
        PROWLARR__AUTH__APIKEY=${serviceCfg.secrets.placeholder.api-key}
      '';
      systemd.dependentServices = [ "prowlarr" "prowlarr-configure" ];
    };
    forwardAuth.enable = true;
    integrations.homepage = {
      enable = true;
      category = "Admin";
      description = "Manage *rr services";
    };
  };

  services.prowlarr = {
    enable = true;
    settings.server.port = serviceCfg.port;
    environmentFiles = [ serviceCfg.secrets.templates."prowlarr.env".path ];
  };

  custom.homelab.cifs.mounts.media.systemd.dependentServices = [ "prowlarr" ];
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
