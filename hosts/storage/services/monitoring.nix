{
  # Bound to every interface but reachable only from compute; the scoping lives in ../firewall.nix.
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
}
