{ config, pkgs, ... }:
let
  port = 9090;
  serviceCfg = config.custom.homelab.services.prometheus;
in
{
  imports = [
    ./alertmanager.nix
    ./grafana.nix
  ];

  custom.homelab.services.prometheus = {
    displayName = "Prometheus";
    metadata.description = "Metrics";
    metadata.version = pkgs.prometheus.version;
    metadata.homepage = pkgs.prometheus.meta.homepage;
    metadata.category = "Monitoring";
    inherit port;
    healthcheck.path = "/-/healthy";
    forwardAuth.enable = true;
    integrations.homepage.enable = true;
    integrations.monitoring = {
      scrapeConfigs = [{
        job_name = "prometheus";
        static_configs = [{
          targets = [ "127.0.0.1:${toString port}" ];
        }];
      }];
      rules = [{
        name = "prometheus";
        rules = [{
          alert = "PrometheusTargetDown";
          expr = "up == 0";
          "for" = "5m";
          labels.severity = "warning";
          annotations.summary = "{{ $labels.job }}/{{ $labels.instance }} down";
        }];
      }];
    };
  };

  # Writes directly to NVMe. SSD wear is negligible with 60s and small set of alerts.
  services.prometheus = {
    enable = true;
    listenAddress = serviceCfg.host;
    port = serviceCfg.port;
    retentionTime = "365d";
    extraFlags = [
      "--storage.tsdb.retention.size=2GB"
      "--storage.tsdb.wal-compression"
    ];
    globalConfig = {
      scrape_interval = "60s";
      evaluation_interval = "60s";
    };
  };
}
