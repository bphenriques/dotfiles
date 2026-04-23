{ config, ... }:
let
  serviceCfg = config.custom.homelab.services.alertmanager;
  ntfyCfg = config.custom.homelab.services.ntfy;
in
{
  custom.homelab.services.alertmanager = {
    displayName = "Alertmanager";
    metadata.description = "Alert Routing";
    metadata.version = config.services.prometheus.alertmanager.package.version;
    metadata.homepage = "https://prometheus.io/docs/alerting/latest/alertmanager/";
    metadata.category = "Monitoring";
    port = 9093;
    healthcheck.path = "/-/healthy";
    forwardAuth.enable = true;
    integrations.homepage.enable = true;
    integrations.ntfy = {
      enable = true;
      topic = "admin";
    };
  };

  services.prometheus = {
    alertmanagers = [{
      static_configs = [{
        targets = [ "127.0.0.1:${toString serviceCfg.port}" ];
      }];
    }];

    alertmanager = {
      enable = true;
      listenAddress = serviceCfg.host;
      inherit (serviceCfg) port;
      configuration = {
        route = {
          receiver = "ntfy";
          group_by = [ "alertname" ];
          group_wait = "30s";
          group_interval = "5m";
          repeat_interval = "4h";
        };
        receivers = [{
          name = "ntfy";
          webhook_configs = [{
            url = "${ntfyCfg.url}/${serviceCfg.integrations.ntfy.topic}?template=alertmanager";
            send_resolved = true;
            http_config.authorization = {
              type = "Bearer";
              credentials_file = "/run/credentials/alertmanager.service/ntfy-token";
            };
          }];
        }];
      };
    };
  };

  systemd.services.alertmanager = {
    after = [ "ntfy-configure.service" ];
    wants = [ "ntfy-configure.service" ];
    serviceConfig.LoadCredential = [ "ntfy-token:${serviceCfg.integrations.ntfy.tokenFile}" ];
  };
}
