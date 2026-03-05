{ config, pkgs, lib, ... }:
let
  serviceCfg = config.custom.homelab.services.prowlarr;
  homelabMounts = config.custom.fileSystems.homelab.mounts;
in
{
  imports = [ ./configure.nix ];

  custom.homelab.services.prowlarr = {
    port = 9096;
    secrets = {
      files.api-key = { rotatable = true; };
      envFile."PROWLARR__AUTH__APIKEY" = "api-key";
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
    environmentFiles = [ serviceCfg.secrets.envFilePath ];
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
