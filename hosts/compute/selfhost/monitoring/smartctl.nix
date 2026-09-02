_:
{
  selfhost.monitoring.scopes.smartctl = {
    exporters.smartctl = {
      enable = true;
      listenAddress = "127.0.0.1";
      port = 9633;
    };
    # Rules match every scraped host, so these cover storage's drives as well as compute's.
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
        {
          alert = "SMARTPendingSectors";
          expr = ''smartctl_device_attribute{attribute_name="Current_Pending_Sector",attribute_value_type="raw"} > 0'';
          "for" = "0m";
          labels.severity = "warning";
          annotations.summary = "{{ $labels.device }}: {{ $value }} sector(s) pending reallocation";
        }
        {
          alert = "SMARTReallocatedSectors";
          expr = ''smartctl_device_attribute{attribute_name="Reallocated_Sector_Ct",attribute_value_type="raw"} > 0'';
          "for" = "0m";
          labels.severity = "warning";
          annotations.summary = "{{ $labels.device }}: {{ $value }} reallocated sector(s)";
        }
        # Split by media: the Toshiba HDDs top out at 55C, NVMe runs far hotter without concern.
        {
          alert = "DiskTooHot";
          expr = ''smartctl_device_temperature{device=~"sd.*",temperature_type="current"} > 50'';
          "for" = "5m";
          labels.severity = "warning";
          annotations.summary = "{{ $labels.device }}: {{ $value }}C";
        }
        {
          alert = "NVMeTooHot";
          expr = ''smartctl_device_temperature{device=~"nvme.*",temperature_type="current"} > 65'';
          "for" = "5m";
          labels.severity = "warning";
          annotations.summary = "{{ $labels.device }}: {{ $value }}C";
        }
      ];
    }];
  };
}
