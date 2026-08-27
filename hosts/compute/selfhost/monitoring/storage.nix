# Scrapes the storage host's exporters. The disk rules themselves live in smartctl.nix and apply to
# every scraped host, so nothing here is storage-specific beyond the pool.
{ config, lib, ... }:
let
  host = config.custom.fleet.lan.hosts.storage;
  target = port: [{
    targets = [ "${host}:${toString port}" ];
    labels.instance = "storage";
  }];
in
{
  selfhost.monitoring.scopes.storage = {
    scrapeConfigs = [
      { job_name = "storage-node"; static_configs = target 9100; }
      { job_name = "storage-smartctl"; scrape_interval = "2m"; static_configs = target 9633; }
      { job_name = "storage-zfs"; static_configs = target 9134; }
    ];

    rules = [{
      name = "storage";
      rules = [
        {
          alert = "ZFSPoolDegraded";
          expr = "zfs_pool_health != 0";
          "for" = "5m";
          labels.severity = "critical";
          annotations.summary = "{{ $labels.pool }}: pool is not ONLINE";
        }
        {
          alert = "ZFSPoolAlmostFull";
          expr = "(1 - zfs_pool_free_bytes / zfs_pool_size_bytes) * 100 > 85";
          "for" = "5m";
          labels.severity = "warning";
          annotations.summary = "{{ $labels.pool }}: {{ $value | printf \"%.0f\" }}% full";
        }
        {
          alert = "StorageExporterDown";
          expr = ''up{job=~"storage-.*"} == 0'';
          "for" = "5m";
          labels.severity = "critical";
          annotations.summary = "{{ $labels.job }}: unreachable";
        }
        {
          # The framework declares this alert on the host serving the shares; that host runs no Prometheus.
          alert = "SmbSharesUnserved";
          expr = ''node_systemd_unit_state{instance="storage",name=~"samba-smbd.service|selfhost-smb-permissions.service",state="failed"} == 1'';
          "for" = "5m";
          labels.severity = "critical";
          annotations.summary = "{{ $labels.name }}: SMB shares are not being served";
        }
      ];
    }];
  };
}
