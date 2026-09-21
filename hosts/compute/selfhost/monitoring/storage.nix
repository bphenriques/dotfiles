# Storage-specific alert rules. The scrape targets live in fleet.nix and the disk rules in
# smartctl.nix, both of which cover every host, so only the pool and the shares are left here.
_:
{
  selfhost.monitoring.scopes.storage = {
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
          # Quota'd datasets are invisible to the pool alert above: the pool has terabytes free while
          # the dataset is full. Inert until a dataset is given a quota.
          alert = "ZFSDatasetQuotaAlmostFull";
          expr = "zfs_dataset_quota_bytes > 0 and (zfs_dataset_used_bytes / zfs_dataset_quota_bytes) * 100 > 85";
          "for" = "15m";
          labels.severity = "warning";
          annotations.summary = "{{ $labels.name }}: {{ $value | printf \"%.0f\" }}% of its quota";
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
