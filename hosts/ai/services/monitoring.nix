{
  # Bound to every interface but reachable only from compute; the scoping lives in ../firewall.nix.
  services.prometheus.exporters = {
    node = {
      enable = true;
      listenAddress = "0.0.0.0";
      enabledCollectors = [ "systemd" ];  # adds to the defaults, which already carry hwmon
    };
    smartctl = {
      enable = true;
      listenAddress = "0.0.0.0";
      maxInterval = "2m";
    };
  };
}
