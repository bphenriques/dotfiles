{ config, ... }:
{
  imports = [
    ./grafana
    ./ups.nix
    ./smartctl.nix
    ./storage.nix
    ./ai.nix
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
          expr = ''(1 - avg by(instance) (rate(node_cpu_seconds_total{mode="idle",instance="compute"}[5m]))) * 100 > 90'';
          "for" = "5m";
          labels.severity = "warning";
          annotations.summary = "CPU > 90%";
        }
        {
          alert = "HighMemory";
          expr = ''(1 - node_memory_MemAvailable_bytes{instance="compute"} / node_memory_MemTotal_bytes{instance="compute"}) * 100 > 85'';
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
          expr = ''max by(instance) (node_hwmon_temp_celsius{instance="compute"}) > 80'';
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
        {
          # Fleet-wide: the named criticals cover specific units, nothing covered "a unit failed".
          # Measured quiet over the last 7d on every host before adding.
          alert = "UnitFailed";
          expr = ''node_systemd_unit_state{state="failed"} == 1'';
          "for" = "15m";
          labels.severity = "warning";
          annotations.summary = "{{ $labels.instance }}: {{ $labels.name }} failed";
        }
      ];
    }];

    systemdOverrides."prometheus-node-exporter".serviceConfig = {
      CapabilityBoundingSet = [ "CAP_DAC_READ_SEARCH" ];
      AmbientCapabilities = [ "CAP_DAC_READ_SEARCH" ];
    };
  };
}