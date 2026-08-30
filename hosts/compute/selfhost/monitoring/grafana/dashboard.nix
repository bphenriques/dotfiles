# Single dashboard for the fleet: an always-visible strip (host, NAS and guests), then one collapsed
# detail row per entity, host first (richest), then the NAS, then each microVM guest. Guest rows are
# generated from config.homelab.microvm.host.guests (the table that also drives scrape/alerts), so
# a new guest needs no edit here. Host and guest CPU/IO share one time axis for easy correlation:
# the guests run on the host's own cores.
{ hostName, guests, storageName, aiName }:
let
  inherit (import ./lib.nix) mkPanel mkStat mkRow layout2 fullW;

  hostInst = ''instance="${hostName}"'';
  storageInst = ''instance="${storageName}"'';
  aiInst = ''instance="${aiName}"'';
  names = builtins.attrNames guests;
  guestInst = builtins.concatStringsSep "|" names;  # RE2 is fully anchored: matches node jobs, not *-traefik
  allInst = "${hostName}|${storageName}|${aiName}|${guestInst}";
  nodeJobs = "node|storage-node|ai-node|${guestInst}";  # host node-exporter job is "node"; each guest's job is its name
  pct = { mode = "absolute"; steps = [ { color = "green"; value = null; } { color = "yellow"; value = 60; } { color = "red"; value = 85; } ]; };

  hostSpecs = [
    {
      id = 101;
      title = "Top Services by Request Rate";
      unit = "reqps";
      legend = "{{service}}";
      expr = ''topk(10, sum by (service) (rate(traefik_service_requests_total{${hostInst}}[5m])))'';
    }
    {
      id = 102;
      title = "WireGuard Peers";
      legend = "{{allowed_ips}}";
      expr = ''(time() - wireguard_latest_handshake_seconds{${hostInst}}) < bool 180'';
    }
    {
      id = 103;
      title = "CPU Usage";
      unit = "percent";
      legend = "CPU %";
      expr = ''(1 - avg by(instance) (rate(node_cpu_seconds_total{${hostInst},mode="idle"}[5m]))) * 100'';
    }
    {
      id = 104;
      title = "Memory Usage";
      unit = "bytes";
      expr = [
        { expr = ''node_memory_MemTotal_bytes{${hostInst}} - node_memory_MemAvailable_bytes{${hostInst}}''; legend = "Used"; }
        { expr = ''node_memory_MemAvailable_bytes{${hostInst}}''; legend = "Available"; }
      ];
    }
    {
      id = 105;
      title = "Hardware Temperatures";
      unit = "celsius";
      legend = "{{chip}} / {{sensor}}";
      expr = ''node_hwmon_temp_celsius{${hostInst}}'';
      thresholds = {
        mode = "absolute";
        steps = [ { color = "green"; value = null; } { color = "yellow"; value = 60; } { color = "red"; value = 80; } ];
      };
    }
    {
      id = 106;
      title = "Power Consumption (RAPL)";
      unit = "watt";
      legend = "{{path}}";
      expr = ''rate(node_rapl_package_joules_total{${hostInst}}[5m])'';
    }
    {
      id = 107;
      title = "Network Bandwidth";
      unit = "Bps";
      expr = [
        { expr = ''sum(rate(node_network_receive_bytes_total{${hostInst},device!~"lo|veth.*|br-.*|docker.*|wg.*"}[5m]))''; legend = "RX"; }
        { expr = ''sum(rate(node_network_transmit_bytes_total{${hostInst},device!~"lo|veth.*|br-.*|docker.*|wg.*"}[5m]))''; legend = "TX"; }
      ];
    }
    {
      id = 108;
      title = "Disk Usage (Root)";
      unit = "bytes";
      expr = [
        { expr = ''node_filesystem_size_bytes{${hostInst},mountpoint="/",fstype!~"tmpfs|overlay|squashfs"}''; legend = "Total"; }
        { expr = ''node_filesystem_size_bytes{${hostInst},mountpoint="/",fstype!~"tmpfs|overlay|squashfs"} - node_filesystem_avail_bytes{${hostInst},mountpoint="/",fstype!~"tmpfs|overlay|squashfs"}''; legend = "Used"; }
      ];
    }
    {
      id = 109;
      title = "NAS Storage Usage";
      unit = "bytes";
      expr = [
        { expr = ''node_filesystem_size_bytes{${hostInst},fstype="cifs"}''; legend = "{{mountpoint}} total"; }
        { expr = ''node_filesystem_size_bytes{${hostInst},fstype="cifs"} - node_filesystem_avail_bytes{${hostInst},fstype="cifs"}''; legend = "{{mountpoint}} used"; }
      ];
    }
  ];

  hostRow = mkRow {
    id = 100;
    title = "${hostName} (host)";
    gridPos = { x = 0; y = 10; w = fullW; h = 1; };
    collapsed = true;
    panels = map mkPanel (layout2 11 hostSpecs);
  };

  # The NAS is scraped by three exporters under one instance label: storage-node, storage-smartctl and
  # storage-zfs. Pool figures come from the zfs exporter, disk temperatures from smartctl.
  storageSpecs = [
    {
      id = 201;
      title = "CPU Usage";
      unit = "percent";
      legend = "CPU %";
      expr = ''(1 - avg by(instance) (rate(node_cpu_seconds_total{${storageInst},mode="idle"}[5m]))) * 100'';
    }
    {
      id = 202;
      title = "Memory Usage";
      unit = "bytes";
      # ARC is kernel slab, so it lands in Used rather than cache; panel 204 breaks it out.
      expr = [
        { expr = ''node_memory_MemTotal_bytes{${storageInst}} - node_memory_MemAvailable_bytes{${storageInst}}''; legend = "Used"; }
        { expr = ''node_memory_MemAvailable_bytes{${storageInst}}''; legend = "Available"; }
      ];
    }
    {
      id = 203;
      title = "ZFS Pool";
      unit = "bytes";
      expr = [
        { expr = ''zfs_pool_size_bytes{${storageInst}}''; legend = "{{pool}} total"; }
        { expr = ''zfs_pool_allocated_bytes{${storageInst}}''; legend = "{{pool}} used"; }
      ];
    }
    {
      id = 204;
      title = "ZFS ARC";
      unit = "bytes";
      expr = [
        { expr = ''node_zfs_arc_size{${storageInst}}''; legend = "ARC size"; }
        { expr = ''node_zfs_arc_c_max{${storageInst}}''; legend = "ARC max"; }
      ];
    }
    {
      id = 205;
      title = "Disk Temperatures";
      unit = "celsius";
      legend = "{{device}}";
      expr = ''smartctl_device_temperature{${storageInst},temperature_type="current"}'';
      # Matches the HDD alert in smartctl.nix, which fires above 50.
      thresholds = {
        mode = "absolute";
        steps = [ { color = "green"; value = null; } { color = "yellow"; value = 45; } { color = "red"; value = 50; } ];
      };
    }
    {
      id = 206;
      title = "Network Bandwidth";
      unit = "Bps";
      expr = [
        { expr = ''sum(rate(node_network_receive_bytes_total{${storageInst},device!~"lo|veth.*|br-.*|docker.*|wg.*"}[5m]))''; legend = "RX"; }
        { expr = ''sum(rate(node_network_transmit_bytes_total{${storageInst},device!~"lo|veth.*|br-.*|docker.*|wg.*"}[5m]))''; legend = "TX"; }
      ];
    }
    {
      id = 207;
      title = "Disk Usage (Root)";
      unit = "bytes";
      expr = [
        { expr = ''node_filesystem_size_bytes{${storageInst},mountpoint="/",fstype!~"tmpfs|overlay|squashfs"}''; legend = "Total"; }
        { expr = ''node_filesystem_size_bytes{${storageInst},mountpoint="/",fstype!~"tmpfs|overlay|squashfs"} - node_filesystem_avail_bytes{${storageInst},mountpoint="/",fstype!~"tmpfs|overlay|squashfs"}''; legend = "Used"; }
      ];
    }
  ];

  storageRow = mkRow {
    id = 200;
    title = "${storageName} (NAS)";
    gridPos = { x = 0; y = 11; w = fullW; h = 1; };
    collapsed = true;
    panels = map mkPanel (layout2 12 storageSpecs);
  };

  # The inference box. hwmon names its chips by PCI path, so every sensor panel joins
  # node_hwmon_sensor_label to get a readable series name; that join also drops unlabelled sensors.
  withLabel = metric: ''
    ${metric}{${aiInst}} * on(chip, sensor) group_left(label) node_hwmon_sensor_label{${aiInst}}
  '';

  aiSpecs = [
    {
      id = 301;
      title = "SoC Package Power (PPT)";
      unit = "watt";
      legend = "{{label}}";
      expr = withLabel "node_hwmon_power_watt";
    }
    {
      id = 302;
      title = "GPU Clock";
      unit = "hertz";
      legend = "{{sensor}}";
      expr = ''node_hwmon_freq_freq_mhz{${aiInst}, sensor="sclk"} * 1000000'';
    }
    {
      id = 303;
      title = "Temperatures";
      unit = "celsius";
      legend = "{{label}}";
      expr = withLabel "node_hwmon_temp_celsius";
      thresholds = {
        mode = "absolute";
        steps = [ { color = "green"; value = null; } { color = "yellow"; value = 75; } { color = "red"; value = 90; } ];
      };
    }
    {
      id = 304;
      title = "CPU Usage";
      unit = "percent";
      legend = "CPU %";
      expr = ''(1 - avg by(instance) (rate(node_cpu_seconds_total{${aiInst},mode="idle"}[5m]))) * 100'';
    }
    {
      id = 305;
      title = "Memory Usage";
      unit = "bytes";
      expr = [
        { expr = ''node_memory_MemTotal_bytes{${aiInst}} - node_memory_MemAvailable_bytes{${aiInst}}''; legend = "Used"; }
        { expr = ''node_memory_MemAvailable_bytes{${aiInst}}''; legend = "Available"; }
      ];
    }
    {
      id = 306;
      title = "Network Bandwidth";
      unit = "Bps";
      expr = [
        { expr = ''rate(node_network_receive_bytes_total{${aiInst},device!="lo"}[5m])''; legend = "{{device}} rx"; }
        { expr = ''rate(node_network_transmit_bytes_total{${aiInst},device!="lo"}[5m])''; legend = "{{device}} tx"; }
      ];
    }
    {
      id = 307;
      title = "Disk Usage (Root)";
      unit = "bytes";
      expr = [
        { expr = ''node_filesystem_size_bytes{${aiInst},mountpoint="/",fstype!~"tmpfs|overlay|squashfs"}''; legend = "Total"; }
        { expr = ''node_filesystem_size_bytes{${aiInst},mountpoint="/",fstype!~"tmpfs|overlay|squashfs"} - node_filesystem_avail_bytes{${aiInst},mountpoint="/",fstype!~"tmpfs|overlay|squashfs"}''; legend = "Used"; }
      ];
    }
  ];

  aiRow = mkRow {
    id = 300;
    title = "${aiName} (inference)";
    gridPos = { x = 0; y = 12; w = fullW; h = 1; };
    collapsed = true;
    panels = map mkPanel (layout2 13 aiSpecs);
  };

  mkVmRow = i: name:
    let
      mon = guests.${name}.monitoring;
      inst = ''instance="${name}"'';
      rowY = 13 + i;  # after the fleet strip (y0..9), host (y10), NAS (y11) and ai (y12)
      base = 10 * (i + 1);
      specs =
        [
          {
            id = base + 1;
            title = "CPU Usage";
            legend = "CPU %";
            unit = "percent";
            expr = ''(1 - avg by(instance) (rate(node_cpu_seconds_total{${inst},mode="idle"}[5m]))) * 100'';
          }
          {
            id = base + 2;
            title = "Memory Usage";
            unit = "bytes";
            expr = [
              { expr = ''node_memory_MemTotal_bytes{${inst}} - node_memory_MemAvailable_bytes{${inst}}''; legend = "Used"; }
              { expr = ''node_memory_MemAvailable_bytes{${inst}}''; legend = "Available"; }
            ];
          }
        ]
        ++ (if mon.storageMount != null then [{
          id = base + 3;
          title = "Storage (${mon.storageMount})";
          unit = "bytes";
          expr = [
            { expr = ''node_filesystem_size_bytes{${inst},mountpoint="${mon.storageMount}",fstype="ext4"}''; legend = "Total"; }
            { expr = ''node_filesystem_size_bytes{${inst},mountpoint="${mon.storageMount}",fstype="ext4"} - node_filesystem_avail_bytes{${inst},mountpoint="${mon.storageMount}",fstype="ext4"}''; legend = "Used"; }
          ];
        }] else [ ])
        ++ (if mon.traefikMetrics then [{
          id = base + 4;
          title = "HTTP responses by status";
          legend = "{{code}}";
          unit = "reqps";
          # entrypoint-level, not service: middleware rejections (auth/rate-limit) never reach a backend.
          # Exclude "metrics" rather than pin "web" (cloudflared serves "tunnel"); this merges a guest's
          # serving entrypoints, which is fine while each guest has exactly one.
          expr = ''sum by (code) (rate(traefik_entrypoint_requests_total{${inst},entrypoint!="metrics"}[5m]))'';
        }] else [ ]);
    in
    mkRow {
      id = base;
      title = name;
      gridPos = { x = 0; y = rowY; w = fullW; h = 1; };
      collapsed = true;
      panels = map mkPanel (layout2 (rowY + 1) specs);
    };
in
{
  uid = "compute";
  title = "Compute";
  tags = [ "compute" "node" "microvm" ];
  timezone = "browser";
  schemaVersion = 39;
  refresh = "1m";
  time = { from = "now-6h"; to = "now"; };
  panels = [
    (mkRow { id = 1; title = "Fleet health"; gridPos = { x = 0; y = 0; w = fullW; h = 1; }; })
    (mkStat {
      id = 2;
      title = "Up";
      expr = ''up{job=~"${nodeJobs}"}'';
      colorMode = "background";
      thresholds = { mode = "absolute"; steps = [ { color = "red"; value = null; } { color = "green"; value = 1; } ]; };
      mappings = [{ type = "value"; options = { "0" = { text = "DOWN"; }; "1" = { text = "UP"; }; }; }];
      gridPos = { x = 0; y = 1; w = 8; h = 5; };
    })
    (mkStat {
      id = 3;
      title = "CPU %";
      unit = "percent";
      expr = ''(1 - avg by(instance) (rate(node_cpu_seconds_total{instance=~"${allInst}",mode="idle"}[5m]))) * 100'';
      thresholds = pct;
      gridPos = { x = 8; y = 1; w = 8; h = 5; };
    })
    (mkStat {
      id = 4;
      title = "Memory %";
      unit = "percent";
      expr = ''(1 - node_memory_MemAvailable_bytes{instance=~"${allInst}"} / node_memory_MemTotal_bytes{instance=~"${allInst}"}) * 100'';
      thresholds = pct;
      gridPos = { x = 16; y = 1; w = 8; h = 5; };
    })
    (mkStat {
      id = 5;
      title = "Pool Free";
      unit = "percent";
      # Percent rather than bytes so the thresholds keep tracking the 85%/90%-full policy at any pool size.
      expr = ''(zfs_pool_free_bytes{${storageInst}} / zfs_pool_size_bytes{${storageInst}}) * 100'';
      legend = "{{pool}}";
      thresholds = {
        mode = "absolute";
        steps = [ { color = "red"; value = null; } { color = "yellow"; value = 10; } { color = "green"; value = 15; } ];
      };
      gridPos = { x = 0; y = 6; w = 12; h = 4; };
    })
    (mkStat {
      id = 6;
      title = "SMART";
      # min: one failing device drops its whole host to FAIL.
      expr = ''min by(instance) (smartctl_device_smart_status)'';
      colorMode = "background";
      thresholds = { mode = "absolute"; steps = [ { color = "red"; value = null; } { color = "green"; value = 1; } ]; };
      mappings = [{ type = "value"; options = { "0" = { text = "FAIL"; }; "1" = { text = "OK"; }; }; }];
      gridPos = { x = 12; y = 6; w = 12; h = 4; };
    })
    hostRow
    storageRow
    aiRow
  ] ++ builtins.genList (i: mkVmRow i (builtins.elemAt names i)) (builtins.length names);
}
