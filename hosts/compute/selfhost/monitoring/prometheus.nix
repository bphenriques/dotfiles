# Replaces the framework's self-scrape to drop ~1200 of Prometheus' 1700 series about itself: Go
# runtime counters and the per-target histograms a sharded deployment needs. By family rather than a
# keep-list, which goes stale the next time one of them is worth reading. The whole job has to be
# restated because scrape configs cannot be appended to.
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
