# ai alert rules, per sensor since parts differ (NVMe crit 89.85C, CPU Tjmax 100C; measured peak 60C/48C). Scrapes in fleet.nix, disks in smartctl.nix.
_:
let
  # hwmon names chips by PCI path, so match node_hwmon_sensor_label on (chip, sensor).
  tempOf = label: ''
    node_hwmon_temp_celsius{instance="ai"}
      * on(chip, sensor) group_left() node_hwmon_sensor_label{instance="ai", label="${label}"}
  '';
in
{
  selfhost.monitoring.scopes.ai = {
    rules = [{
      name = "ai";
      rules = [
        {
          alert = "AICpuOverheating";
          expr = "${tempOf "Tctl"} > 95";
          "for" = "10m";
          labels.severity = "warning";
          annotations.summary = ''ai CPU at {{ $value | printf "%.0f" }}C, throttles at 100C'';
        }
        {
          alert = "AIGpuOverheating";
          expr = "${tempOf "edge"} > 90";
          "for" = "10m";
          labels.severity = "warning";
          annotations.summary = ''ai GPU at {{ $value | printf "%.0f" }}C'';
        }
        {
          alert = "AIDiskOverheating";
          expr = "${tempOf "Composite"} > 75";
          "for" = "10m";
          labels.severity = "warning";
          annotations.summary = ''ai NVMe at {{ $value | printf "%.0f" }}C, drive throttles at 86C'';
        }
      ];
    }];
  };
}
