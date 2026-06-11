{ config, pkgs, lib, ... }:
let
  serviceCfg = config.selfhost.services.prowlarr;
in
{
  imports = [ ./configure.nix ];

  selfhost = {
    services.prowlarr = {
      displayName = "Prowlarr";
      description = "Manage *rr services";
      port = 9096;
      healthcheck.path = "/ping";
      access.allowedGroups = [ config.selfhost.groups.admin ];
      forwardAuth.enable = true;
      integrations.homepage.group = "Admin";
      integrations.notify.enable = true;
      integrations.notify.topic = "admin";
    };

    runtimeSecrets.prowlarr-api-key = {
      restartUnits = [ "prowlarr.service" "prowlarr-configure.service" ];
    };

    runtimeTemplates."prowlarr.env" = {
      content = ''
        PROWLARR__AUTH__APIKEY=${config.selfhost.runtimePlaceholder.prowlarr-api-key}
      '';
      restartUnits = [ "prowlarr.service" ];
    };
  };

  services.prowlarr = {
    enable = true;
    settings.server.port = serviceCfg.port;
    settings.server.bindaddress = "127.0.0.1";
    environmentFiles = [ config.selfhost.runtimeTemplates."prowlarr.env".path ];
  };

  # Prowlarr is an indexer manager: it talks to APIs, not the filesystem. No media mount needed.
  systemd.services.prowlarr = {
    serviceConfig = {
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
