{
  # Only hwmon and systemd: it already carries every sensor plus SoC package power (PPT) and GPU
  # clock. rapl needs root to read energy_uj, and thermal_zone returns nothing on this board.
  services.prometheus.exporters.node = {
    enable = true;
    listenAddress = "0.0.0.0";
    port = 9100;
    enabledCollectors = [
      "hwmon"
      "systemd"
    ];
  };
}
