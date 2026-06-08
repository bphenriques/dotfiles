{ config, ... }:
{
  imports = [
    ./grafana
    ./ups.nix
    ./smartctl.nix
  ];

  selfhost.monitoring.scopes.node = {
    exporters.node = {
      enable = true;
      listenAddress = "127.0.0.1";
      port = 9101;
      enabledCollectors = [ "hwmon" "rapl" "systemd" "thermal_zone" ];
    };

    scrapeConfigs = [{
      job_name = "node";
      static_configs = [{
        targets = [ "127.0.0.1:9101" ];
        labels.instance = config.networking.hostName;
      }];
    }];

    rules = [{
      name = "system";
      rules = [
        {
          alert = "HighCPU";
          expr = ''(1 - avg by(instance) (rate(node_cpu_seconds_total{mode="idle"}[5m]))) * 100 > 90'';
          "for" = "5m";
          labels.severity = "warning";
          annotations.summary = "CPU > 90%";
        }
        {
          alert = "HighMemory";
          expr = "(1 - node_memory_MemAvailable_bytes / node_memory_MemTotal_bytes) * 100 > 85";
          "for" = "5m";
          labels.severity = "warning";
          annotations.summary = "Memory > 85%";
        }
        {
          alert = "DiskAlmostFull";
          expr = ''(1 - node_filesystem_avail_bytes{mountpoint="/",fstype!~"tmpfs|overlay|squashfs"} / node_filesystem_size_bytes{mountpoint="/",fstype!~"tmpfs|overlay|squashfs"}) * 100 > 80'';
          "for" = "5m";
          labels.severity = "critical";
          annotations.summary = "Disk > 80%";
        }
        {
          alert = "HighTemperature";
          expr = "max by(instance) (node_hwmon_temp_celsius) > 80";
          "for" = "30s";
          labels.severity = "critical";
          annotations.summary = "Temp > 80°C";
        }
        {
          alert = "NASStorageFull";
          expr = ''(1 - node_filesystem_avail_bytes{fstype="cifs"} / node_filesystem_size_bytes{fstype="cifs"}) * 100 > 85'';
          "for" = "5m";
          labels.severity = "warning";
          annotations.summary = "{{ $labels.mountpoint }} > 85%";
        }
      ];
    }];

    systemdOverrides."prometheus-node-exporter".serviceConfig = {
      CapabilityBoundingSet = [ "CAP_DAC_READ_SEARCH" ];
      AmbientCapabilities = [ "CAP_DAC_READ_SEARCH" ];
    };
  };
}