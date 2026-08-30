{ config, pkgs, ... }:
let
  serviceCfg = config.selfhost.services.grafana;
  prometheusCfg = config.selfhost.services.prometheus;
  json = pkgs.formats.json { };

  computeDashboard = json.generate "compute.json" (import ./dashboard.nix {
    hostName = config.networking.hostName;
    guests = config.homelab.microvm.host.guests;
    storageName = "storage"; # the instance label ../storage.nix attaches to the NAS scrape jobs
    aiName = "ai";           # likewise ../ai.nix
  });
in
{
  selfhost = {
    services.grafana = {
      displayName = "Grafana";
      meta.homepage = "https://grafana.com";
      meta.description = "Dashboards";
      meta.category = "monitoring";
      port = 3010;
      healthcheck.path = "/api/health";
      access.model = "forwardAuth";
      integrations.homepage.group = "Admin";
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
      dashboards.default_home_dashboard_path = "${computeDashboard}";
      security.secret_key = "$__file{${config.selfhost.runtimeSecrets.grafana-secret-key.path}}";
      users.allow_sign_up = false;
      "auth.anonymous" = {
        enabled = true; # Fine as Grafana is behind forwardAuth and it is view only
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
        options.path = pkgs.linkFarm "grafana-dashboards" [
          { name = "compute.json"; path = computeDashboard; }
        ];
      }];
    };
  };
}
