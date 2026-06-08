{
  imports = [ ./grafana.nix ];

  custom.homelab.monitoring = {
    enable = true;
    alertmanager.enable = true;
    retentionTime = "365d";
    retentionSize = "5GB";   # ~15 targets × ~200 metrics × 60s ≈ 4.7 GB/year; size is the effective bound
    scrapeInterval = "60s";
  };
}
