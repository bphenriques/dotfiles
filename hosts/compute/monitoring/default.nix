{ config, pkgs, ... }:
let
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
    port = 9090;
    healthcheck.path = "/-/healthy";
    forwardAuth.enable = true;
    integrations.homepage.enable = true;
    integrations.monitoring = {
      scrapeConfigs = [{
        job_name = "prometheus";
        scrape_interval = "300s";
        static_configs = [{
          targets = [ "127.0.0.1:${toString serviceCfg.port}" ];
        }];
      }];
      rules = [{
        name = "prometheus";
        rules = [{
          alert = "PrometheusTargetDown";
          expr = "up == 0";
          "for" = "10m";
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
    inherit (serviceCfg) port;
    # Both limits apply; size is the effective bound (~15 targets × ~200 metrics × 60s × ~3 bytes/sample ≈ 13 MB/day ≈ 4.7 GB/year)
    retentionTime = "365d";
    extraFlags = [
      "--storage.tsdb.retention.size=5GB"
      "--storage.tsdb.wal-compression"
    ];
    globalConfig = {
      scrape_interval = "60s";
      evaluation_interval = "60s";
    };
  };
}
