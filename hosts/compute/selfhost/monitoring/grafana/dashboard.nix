let
  inherit (import ./lib.nix) mkPanel mkRow h w fullW;
in
{
  uid = "system-overview";
  title = "System Overview";
  tags = [ "system" "node" ];
  timezone = "browser";
  schemaVersion = 39;
  refresh = "1m";
  time = { from = "now-6h"; to = "now"; };
  panels = [
    # --- Traffic ---
    (mkRow { id = 1; title = "Traffic"; gridPos = { x = 0; y = 0; w = fullW; h = 1; }; })
    (mkPanel {
      id = 2;
      title = "Top Services by Request Rate";
      expr = ''topk(10, sum by (service) (rate(traefik_service_requests_total[5m])))'';
      legend = "{{service}}";
      unit = "reqps";
      gridPos = { x = 0; y = 1; inherit w h; };
    })
    (mkPanel {
      id = 3;
      title = "WireGuard Peers";
      expr = ''(time() - wireguard_latest_handshake_seconds) < bool 180'';
      legend = "{{allowed_ips}}";
      gridPos = { x = w; y = 1; inherit w h; };
    })

    # --- CPU, Memory, Power & Network ---
    (mkRow { id = 4; title = "CPU, Memory, Power & Network"; gridPos = { x = 0; y = 9; w = fullW; h = 1; }; })
    (mkPanel {
      id = 5;
      title = "CPU Usage";
      expr = ''(1 - avg by(instance) (rate(node_cpu_seconds_total{mode="idle"}[5m]))) * 100'';
      legend = "CPU %";
      unit = "percent";
      gridPos = { x = 0; y = 10; inherit w h; };
    })
    (mkPanel {
      id = 6;
      title = "Memory Usage";
      expr = [
        { expr = "node_memory_MemTotal_bytes - node_memory_MemAvailable_bytes"; legend = "Used"; }
        { expr = "node_memory_MemAvailable_bytes"; legend = "Available"; }
      ];
      unit = "bytes";
      gridPos = { x = w; y = 10; inherit w h; };
    })
    (mkPanel {
      id = 7;
      title = "Hardware Temperatures";
      expr = "node_hwmon_temp_celsius";
      legend = "{{chip}} / {{sensor}}";
      unit = "celsius";
      gridPos = { x = 0; y = 18; inherit w h; };
      thresholds = {
        mode = "absolute";
        steps = [
          { color = "green"; value = null; }
          { color = "yellow"; value = 60; }
          { color = "red"; value = 80; }
        ];
      };
    })
    (mkPanel {
      id = 8;
      title = "Power Consumption (RAPL)";
      expr = ''rate(node_rapl_package_joules_total[5m])'';
      legend = "{{path}}";
      unit = "watt";
      gridPos = { x = w; y = 18; inherit w h; };
    })
    (mkPanel {
      id = 9;
      title = "Network Bandwidth";
      expr = [
        { expr = ''sum(rate(node_network_receive_bytes_total{device!~"lo|veth.*|br-.*|docker.*|wg.*"}[5m]))''; legend = "RX"; }
        { expr = ''sum(rate(node_network_transmit_bytes_total{device!~"lo|veth.*|br-.*|docker.*|wg.*"}[5m]))''; legend = "TX"; }
      ];
      unit = "Bps";
      gridPos = { x = 0; y = 26; inherit w h; };
    })

    # --- Disk ---
    (mkRow { id = 10; title = "Disk"; gridPos = { x = 0; y = 34; w = fullW; h = 1; }; })
    (mkPanel {
      id = 11;
      title = "Disk Usage (Root)";
      expr = [
        { expr = ''node_filesystem_size_bytes{mountpoint="/",fstype!~"tmpfs|overlay|squashfs"}''; legend = "Total"; }
        { expr = ''node_filesystem_size_bytes{mountpoint="/",fstype!~"tmpfs|overlay|squashfs"} - node_filesystem_avail_bytes{mountpoint="/",fstype!~"tmpfs|overlay|squashfs"}''; legend = "Used"; }
      ];
      unit = "bytes";
      gridPos = { x = 0; y = 35; inherit w h; };
    })
    (mkPanel {
      id = 12;
      title = "NAS Storage Usage";
      expr = [
        { expr = ''node_filesystem_size_bytes{fstype="cifs"}''; legend = "{{mountpoint}} total"; }
        { expr = ''node_filesystem_size_bytes{fstype="cifs"} - node_filesystem_avail_bytes{fstype="cifs"}''; legend = "{{mountpoint}} used"; }
      ];
      unit = "bytes";
      gridPos = { x = w; y = 35; inherit w h; };
    })

  ];
}
