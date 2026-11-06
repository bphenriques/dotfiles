# Replaces the self-scrape to drop ~1200 of 1700 self series by family. Restated whole, since scrape configs cannot be appended to.
{ config, ... }:
{
  selfhost.services.prometheus.integrations.monitoring.scrapeConfigs = [{
    job_name = "prometheus";
    scrape_interval = "300s";
    static_configs = [{ targets = [ "127.0.0.1:${toString config.selfhost.services.prometheus.port}" ]; }];
    metric_relabel_configs = [{
      source_labels = [ "__name__" ];
      regex = "(go|process|promhttp|net_conntrack|prometheus_(sd|http|target))_.*";
      action = "drop";
    }];
  }];
}
