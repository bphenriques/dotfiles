{ config, pkgs, ... }:
let
  serviceCfg = config.selfhost.services.grafana;
  prometheusCfg = config.selfhost.services.prometheus;
  json = pkgs.formats.json { };

  systemDashboard = json.generate "system.json" (import ./dashboard.nix);
in
{
  selfhost = {
    services.grafana = {
      displayName = "Grafana";
      description = "Dashboards";
      port = 3010;
      healthcheck.path = "/api/health";
      forwardAuth.enable = true;
      integrations.homepage = { enable = true; tab = "Admin"; };
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
      security.secret_key = "$__file{${config.selfhost.runtimeSecrets.grafana-secret-key.path}}";
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
        name = "selfhost";
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
