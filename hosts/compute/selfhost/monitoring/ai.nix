# ai-specific alert rules; the scrape targets live in fleet.nix. Temperature rules are per sensor
# rather than the fleet-wide `max(node_hwmon_temp_celsius) > 80`, which takes a max across parts with
# very different limits: NVMe crit is 89.85C against a CPU Tjmax of 100C. Measured peak under
# inference is CPU 60C / GPU 48C, so these thresholds sit well above normal load and below the point
# each part protects itself. Disk rules come from smartctl.nix, which is host-agnostic.
_:
let
  # hwmon exposes chips by PCI path, so match the human label instead: node_hwmon_sensor_label
  # carries it and joins on (chip, sensor).
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
