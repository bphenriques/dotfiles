{ config, pkgs, ... }:
let
  serviceCfg = config.custom.homelab.services.prometheus;
in
{
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
