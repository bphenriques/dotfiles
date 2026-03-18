# System dashboard: CPU, memory, disk, temperature, power, network, NVMe health
let
  datasource = { type = "prometheus"; uid = "prometheus"; };

  h = 8;
  w = 12;
  fullW = 24;

  mkPanel = { id, title, expr, unit ? "short", type ? "timeseries", gridPos, legendMode ? "list", thresholds ? null, ... }@args: {
    inherit id title type gridPos datasource;
    fieldConfig.defaults = {
      inherit unit;
      custom = {
        lineWidth = 1;
        fillOpacity = 10;
        spanNulls = true;
      };
    } // (if thresholds != null then { inherit thresholds; } else {});
    options = {
      tooltip.mode = "multi";
      legend = { displayMode = legendMode; placement = "bottom"; };
    };
    targets = if builtins.isList expr then
      builtins.genList (i: {
        inherit datasource;
        refId = builtins.elemAt [ "A" "B" "C" "D" "E" "F" "G" "H" ] i;
        expr = (builtins.elemAt expr i).expr;
        legendFormat = (builtins.elemAt expr i).legend;
      }) (builtins.length expr)
    else
      [{ inherit datasource; refId = "A"; inherit expr; legendFormat = args.legend or ""; }];
  };

  mkStat = { id, title, expr, unit ? "short", gridPos, thresholds ? null, ... }@args: {
    inherit id title gridPos datasource;
    type = "stat";
    fieldConfig.defaults = {
      inherit unit;
    } // (if thresholds != null then { inherit thresholds; } else {});
    targets = [{ inherit datasource; refId = "A"; inherit expr; legendFormat = args.legend or ""; }];
    options.reduceOptions = { calcs = [ "lastNotNull" ]; fields = ""; values = false; };
  };

  mkRow = { id, title, gridPos }: {
    inherit id title gridPos;
    type = "row";
    collapsed = false;
  };
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
    # --- CPU, Memory & Power ---
    (mkRow { id = 1; title = "CPU, Memory & Power"; gridPos = { x = 0; y = 0; w = fullW; h = 1; }; })
    (mkPanel {
      id = 2;
      title = "CPU Usage";
      expr = ''(1 - avg by(instance) (rate(node_cpu_seconds_total{mode="idle"}[5m]))) * 100'';
      legend = "CPU %";
      unit = "percent";
      gridPos = { x = 0; y = 1; inherit w h; };
    })
    (mkPanel {
      id = 3;
      title = "Memory Usage";
      expr = [
        { expr = "node_memory_MemTotal_bytes - node_memory_MemAvailable_bytes"; legend = "Used"; }
        { expr = "node_memory_MemAvailable_bytes"; legend = "Available"; }
      ];
      unit = "bytes";
      gridPos = { x = w; y = 1; inherit w h; };
    })
    (mkPanel {
      id = 4;
      title = "Hardware Temperatures";
      expr = "node_hwmon_temp_celsius";
      legend = "{{chip}} / {{sensor}}";
      unit = "celsius";
      gridPos = { x = 0; y = 9; inherit w h; };
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
      id = 5;
      title = "Power Consumption (RAPL)";
      expr = ''rate(node_rapl_package_joules_total[5m])'';
      legend = "{{path}}";
      unit = "watt";
      gridPos = { x = w; y = 9; inherit w h; };
    })

    # --- Disk ---
    (mkRow { id = 6; title = "Disk"; gridPos = { x = 0; y = 17; w = fullW; h = 1; }; })
    (mkPanel {
      id = 7;
      title = "Disk Usage (Root)";
      expr = [
        { expr = ''node_filesystem_size_bytes{mountpoint="/",fstype!~"tmpfs|overlay|squashfs"}''; legend = "Total"; }
        { expr = ''node_filesystem_size_bytes{mountpoint="/",fstype!~"tmpfs|overlay|squashfs"} - node_filesystem_avail_bytes{mountpoint="/",fstype!~"tmpfs|overlay|squashfs"}''; legend = "Used"; }
      ];
      unit = "bytes";
      gridPos = { x = 0; y = 18; inherit w h; };
    })
    (mkPanel {
      id = 8;
      title = "NAS Storage Usage";
      expr = [
        { expr = ''node_filesystem_size_bytes{fstype="cifs"}''; legend = "{{mountpoint}} total"; }
        { expr = ''node_filesystem_size_bytes{fstype="cifs"} - node_filesystem_avail_bytes{fstype="cifs"}''; legend = "{{mountpoint}} used"; }
      ];
      unit = "bytes";
      gridPos = { x = w; y = 18; inherit w h; };
    })

    # --- NVMe Health ---
    (mkRow { id = 9; title = "NVMe Health"; gridPos = { x = 0; y = 26; w = fullW; h = 1; }; })
    (mkStat {
      id = 10;
      title = "SMART Status";
      expr = "smartctl_device_smart_status";
      legend = "{{device}}";
      gridPos = { x = 0; y = 27; inherit w; h = 4; };
      thresholds = {
        mode = "absolute";
        steps = [
          { color = "red"; value = null; }
          { color = "green"; value = 1; }
        ];
      };
    })
    (mkStat {
      id = 11;
      title = "Wear Level %";
      expr = "smartctl_device_percentage_used";
      legend = "{{device}}";
      unit = "percent";
      gridPos = { x = w; y = 27; inherit w; h = 4; };
      thresholds = {
        mode = "absolute";
        steps = [
          { color = "green"; value = null; }
          { color = "yellow"; value = 50; }
          { color = "red"; value = 80; }
        ];
      };
    })

    # --- Network ---
    (mkRow { id = 12; title = "Network"; gridPos = { x = 0; y = 31; w = fullW; h = 1; }; })
    (mkStat {
      id = 13;
      title = "WireGuard Peers";
      expr = ''count(time() - wireguard_latest_handshake_seconds < 180)'';
      legend = "connected";
      gridPos = { x = 0; y = 32; w = 6; h = 4; };
    })
    (mkPanel {
      id = 14;
      title = "Network Traffic";
      expr = [
        { expr = ''rate(node_network_receive_bytes_total{device!~"lo|veth.*|br-.*|docker.*"}[5m])''; legend = "{{device}} rx"; }
        { expr = ''- rate(node_network_transmit_bytes_total{device!~"lo|veth.*|br-.*|docker.*"}[5m])''; legend = "{{device}} tx"; }
      ];
      unit = "Bps";
      gridPos = { x = 6; y = 32; w = 18; inherit h; };
    })
  ];
}
