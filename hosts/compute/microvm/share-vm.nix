{ config, ... }:
{
  selfhost.monitoring.scopes.share-vm = {
    scrapeConfigs = [
      {
        job_name = "share-vm";
        static_configs = [{
          targets = [ "${config.custom.fleet.computeMicrovm.hosts.share-vm}:9100" ];
          labels.instance = "share-vm";
        }];
      }
      {
        job_name = "share-vm-traefik";
        static_configs = [{
          targets = [ "${config.custom.fleet.computeMicrovm.hosts.share-vm}:9117" ];
          labels.instance = "share-vm";
        }];
      }
    ];
    rules = [{
      name = "share-vm";
      rules = [
        {
          alert = "ShareVmDown";
          expr = ''up{job="share-vm"} == 0'';
          for = "5m";
          labels.severity = "warning";
          annotations.summary = "share-vm is unreachable — file sharing is offline";
        }
        {
          alert = "ShareStorageNearCap";
          expr = ''node_filesystem_avail_bytes{mountpoint="/srv/share",fstype="ext4"} / node_filesystem_size_bytes{mountpoint="/srv/share",fstype="ext4"} < 0.1'';
          for = "15m";
          labels.severity = "warning";
          annotations.summary = "/srv/share is over 90% full — uploads will start failing";
        }
        {
          alert = "ShareVmHighCpu";
          expr = ''(1 - avg by(instance) (rate(node_cpu_seconds_total{instance="share-vm",mode="idle"}[5m]))) * 100 > 90'';
          for = "15m";
          labels.severity = "warning";
          annotations.summary = "share-vm CPU over 90% for 15m";
        }
      ];
    }];
  };
}
