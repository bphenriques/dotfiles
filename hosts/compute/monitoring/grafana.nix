{ config, pkgs, ... }:
let
  serviceCfg = config.custom.homelab.services.grafana;
  prometheusCfg = config.custom.homelab.services.prometheus;
  json = pkgs.formats.json { };

  systemDashboard = json.generate "system.json" (import ./grafana-dashboard.nix);
in
{
  custom.homelab = {
    services.grafana = {
      displayName = "Grafana";
      description = "Dashboards";
      category = "Monitoring";
      port = 3010;
      healthcheck.path = "/api/health";
      forwardAuth.enable = true;
      integrations.homepage.enable = true;
    };

    runtimeSecrets.grafana-secret-key = {
      owner = "grafana";
      restartUnits = [ "grafana.service" ];
    };
  };

  services.grafana = {
    enable = true;
    settings = {
      server = {
        http_addr = serviceCfg.host;
        http_port = serviceCfg.port;
        domain = serviceCfg.publicHost;
        root_url = serviceCfg.publicUrl;
      };
      analytics.reporting_enabled = false;
      "unified_alerting".enabled = false;
      alerting.enabled = false; # Already using Alert Manager
      dashboards.default_home_dashboard_path = "${systemDashboard}";
      security.secret_key = "$__file{${config.custom.homelab.runtimeSecrets.grafana-secret-key.path}}";
      users.allow_sign_up = false;

      # Anonymous auth is safe here: Grafana is behind forwardAuth, so all users are already
      # authenticated via the proxy. This sets the default Grafana role for those users.
      "auth.anonymous" = {
        enabled = true;
        org_role = "Viewer";
      };
    };

    provision = {
      enable = true;
      datasources.settings.datasources = [{
        name = "Prometheus";
        uid = "prometheus";
        type = "prometheus";
        inherit (prometheusCfg) url;
        isDefault = true;
        editable = false;
      }];

      dashboards.settings.providers = [{
        name = "homelab";
        type = "file";
        disableDeletion = true;
        options.path = pkgs.linkFarm "grafana-dashboards" [{
          name = "system.json";
          path = systemDashboard;
        }];
      }];
    };
  };
}
