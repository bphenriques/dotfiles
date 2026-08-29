{ config, ... }:
{
  services.prometheus.exporters = {
    node = {
      enable = true;
      listenAddress = "0.0.0.0";
      port = 9100;
      enabledCollectors = [
        "hwmon"
        "systemd"
      ];
    };
    smartctl = {
      enable = true;
      listenAddress = "0.0.0.0";
      port = 9633;
      maxInterval = "2m";
    };
    zfs = {
      enable = true;
      listenAddress = "0.0.0.0";
      port = 9134;
    };
  };

  networking.firewall.allowedTCPPorts = [
    config.services.prometheus.exporters.node.port
    config.services.prometheus.exporters.smartctl.port
    config.services.prometheus.exporters.zfs.port
  ];
}
