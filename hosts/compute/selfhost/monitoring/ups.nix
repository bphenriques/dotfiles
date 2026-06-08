{ lib, pkgs, config, ... }:
let
  upsmon = config.power.ups.upsmon.monitor.synology;
  parsed = builtins.match "([^@]+)@(.+)" upsmon.system;
  upsName = builtins.elemAt parsed 0;
  nutServer = builtins.elemAt parsed 1;
in
{
  selfhost.monitoring.scopes.ups = lib.mkIf (config.power.ups.enable && config.power.ups.upsmon.monitor ? synology) (
    assert lib.assertMsg (parsed != null)
      "power.ups.upsmon.monitor.synology.system must be \"ups@host\" (got: ${upsmon.system})";
  {
    exporters.nut = {
      enable = true;
      listenAddress = "127.0.0.1";
      port = 9305;
      inherit nutServer;
      nutUser = upsmon.user;
      passwordPath = upsmon.passwordFile;
    };
    scrapeConfigs = [{
      job_name = "nut";
      metrics_path = "/ups_metrics";
      params.ups = [ upsName ];
      static_configs = [{
        targets = [ "127.0.0.1:9305" ];
        labels.instance = config.networking.hostName;
      }];
    }];
    rules = [{
      name = "ups";
      rules = [
        {
          alert = "UPSOnBattery";
          expr = ''network_ups_tools_ups_status{flag="OB"} == 1'';
          "for" = "0m";
          labels.severity = "critical";
          annotations.summary = "UPS running on battery";
        }
        {
          alert = "UPSLowBattery";
          expr = ''network_ups_tools_ups_status{flag="LB"} == 1'';
          "for" = "0m";
          labels.severity = "critical";
          annotations.summary = "UPS battery low";
        }
        {
          alert = "NUTExporterDown";
          expr = ''up{job="nut"} == 0'';
          "for" = "5m";
          labels.severity = "critical";
          annotations.summary = "NUT exporter unreachable";
        }
      ];
    }];
    systemdOverrides."prometheus-nut-exporter".serviceConfig.SupplementaryGroups = [ "keys" ];
  });
}
