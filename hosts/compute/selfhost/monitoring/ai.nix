# Scrapes the ai host's node exporter. Temperature rules are per sensor rather than the fleet-wide
# `max(node_hwmon_temp_celsius) > 80`: an APU under sustained inference sits in the 70-90C band by
# design, so that rule would fire continuously here. Thresholds below are set above normal load and
# below the point each part protects itself, so firing means cooling has actually degraded.
{ config, ... }:
let
  host = config.custom.fleet.lan.hosts.ai;

  # hwmon exposes chips by PCI path, so match the human label instead: node_hwmon_sensor_label
  # carries it and joins on (chip, sensor).
  tempOf = label: ''
    node_hwmon_temp_celsius{instance="ai"}
      * on(chip, sensor) group_left() node_hwmon_sensor_label{instance="ai", label="${label}"}
  '';
in
{
  selfhost.monitoring.scopes.ai = {
    scrapeConfigs = [{
      job_name = "ai-node";
      static_configs = [{
        targets = [ "${host}:9100" ];
        labels.instance = "ai";
      }];
    }];

    rules = [{
      name = "ai";
      rules = [
        {
          alert = "AIExporterDown";
          expr = ''up{job="ai-node"} == 0'';
          "for" = "5m";
          labels.severity = "critical";
          annotations.summary = "ai: unreachable";
        }
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
