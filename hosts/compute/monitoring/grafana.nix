{ config, pkgs, lib, ... }:
let
  serviceCfg = config.custom.homelab.services.grafana;
  prometheusCfg = config.custom.homelab.services.prometheus;
  json = pkgs.formats.json { };

  systemDashboard = json.generate "system.json" (import ./dashboards/system.nix);

  dashboardDir = pkgs.linkFarm "grafana-dashboards" [{
    name = "system.json";
    path = systemDashboard;
  }];
in
{
  custom.homelab.services.grafana = {
    metadata.description = "Dashboards";
    metadata.version = pkgs.grafana.version;
    metadata.homepage = pkgs.grafana.meta.homepage;
    metadata.category = "Monitoring";
    port = 3010;
    secrets = {
      files.secret-key.rotatable = true;
      systemd.dependentServices = [ "grafana" ];
    };
    healthcheck.path = "/api/health";
    forwardAuth.enable = true;
    integrations.homepage.enable = true;
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
      dashboards.default_home_dashboard_path = "${systemDashboard}";
      security.secret_key = "$__file{${serviceCfg.secrets.files.secret-key.path}}";
      users.allow_sign_up = false;

      # Already gated using forward auth
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
        url = "http://${prometheusCfg.host}:${toString prometheusCfg.port}";
        isDefault = true;
        editable = false;
      }];

      dashboards.settings.providers = [{
        name = "homelab";
        type = "file";
        disableDeletion = true;
        options.path = dashboardDir;
      }];
    };
  };
}
