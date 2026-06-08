{ config, ... }:
{
  selfhost.monitoring.scopes.smartctl = {
    exporters.smartctl = {
      enable = true;
      listenAddress = "127.0.0.1";
      port = 9633;
    };
    scrapeConfigs = [{
      job_name = "smartctl";
      scrape_interval = "15m";
      static_configs = [{
        targets = [ "127.0.0.1:9633" ];
        labels.instance = config.networking.hostName;
      }];
    }];
    rules = [{
      name = "disk-health";
      rules = [
        {
          alert = "SMARTDiskUnhealthy";
          expr = "smartctl_device_smart_status == 0";
          "for" = "0m";
          labels.severity = "critical";
          annotations.summary = "{{ $labels.device }}: SMART unhealthy";
        }
        {
          alert = "SMARTHighWearLevel";
          expr = "smartctl_device_percentage_used > 80";
          "for" = "0m";
          labels.severity = "warning";
          annotations.summary = "{{ $labels.device }}: {{ $value }}% wear";
        }
        {
          alert = "SMARTCriticalWarning";
          expr = "smartctl_device_critical_warning > 0";
          "for" = "0m";
          labels.severity = "critical";
          annotations.summary = "{{ $labels.device }}: SMART critical warning";
        }
      ];
    }];
  };
}
