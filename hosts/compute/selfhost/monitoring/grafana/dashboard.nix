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

  # Every entity gets these two; only the instance selector changes.
  common = base: inst: [
    {
      id = base + 1;
      title = "CPU Usage";
      unit = "percent";
      legend = "CPU %";
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
  ];

  # Hosts only: a guest's root is tmpfs, which this filter excludes by design.
  rootDisk = id: inst: {
    inherit id;
    title = "Disk Usage (Root)";
    unit = "bytes";
    expr = [
      { expr = ''node_filesystem_size_bytes{${inst},mountpoint="/",fstype!~"tmpfs|overlay|squashfs"}''; legend = "Total"; }
      { expr = ''node_filesystem_size_bytes{${inst},mountpoint="/",fstype!~"tmpfs|overlay|squashfs"} - node_filesystem_avail_bytes{${inst},mountpoint="/",fstype!~"tmpfs|overlay|squashfs"}''; legend = "Used"; }
    ];
  };

  hostSpecs = common 100 hostInst ++ [
    (rootDisk 103 hostInst)
    {
      id = 104;
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
      id = 105;
      title = "Power Consumption (RAPL)";
      unit = "watt";
      legend = "{{path}}";
      expr = ''rate(node_rapl_package_joules_total{${hostInst}}[5m])'';
    }
    {
      id = 106;
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
    gridPos = { x = 0; y = 6; w = fullW; h = 1; };
    collapsed = true;
    panels = map mkPanel (layout2 7 hostSpecs);
  };

  # The NAS answers on one instance label across the node, smartctl and zfs jobs. Pool figures come
  # from the zfs exporter, disk temperatures from smartctl. Its memory panel counts the ZFS ARC under
  # Used rather than cache, because ARC is kernel slab.
  storageSpecs = common 200 storageInst ++ [
    (rootDisk 203 storageInst)
    {
      id = 204;
      title = "ZFS Pool";
      unit = "bytes";
      expr = [
        { expr = ''zfs_pool_size_bytes{${storageInst}}''; legend = "{{pool}} total"; }
        { expr = ''zfs_pool_allocated_bytes{${storageInst}}''; legend = "{{pool}} used"; }
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
  ];

  storageRow = mkRow {
    id = 200;
    title = "${storageName} (NAS)";
    gridPos = { x = 0; y = 7; w = fullW; h = 1; };
    collapsed = true;
    panels = map mkPanel (layout2 8 storageSpecs);
  };

  # The inference box. hwmon names its chips by PCI path, so every sensor panel joins
  # node_hwmon_sensor_label to get a readable series name; that join also drops unlabelled sensors.
  # Job-pinned: a renamed job leaves its old series in the index, and two matches per (chip, sensor)
  # make this a many-to-one error rather than a graph.
  withLabel = metric: ''
    ${metric}{${aiInst},job="node"} * on(chip, sensor) group_left(label) node_hwmon_sensor_label{${aiInst},job="node"}
  '';

  aiSpecs = common 300 aiInst ++ [
    (rootDisk 303 aiInst)
    {
      id = 304;
      title = "SoC Package Power (PPT)";
      unit = "watt";
      legend = "{{label}}";
      expr = withLabel "node_hwmon_power_watt";
    }
    {
      id = 305;
      title = "GPU Clock";
      unit = "hertz";
      legend = "{{sensor}}";
      expr = ''node_hwmon_freq_freq_mhz{${aiInst}, sensor="sclk"} * 1000000'';
    }
    {
      id = 306;
      title = "Temperatures";
      unit = "celsius";
      legend = "{{label}}";
      expr = withLabel "node_hwmon_temp_celsius";
      thresholds = {
        mode = "absolute";
        steps = [ { color = "green"; value = null; } { color = "yellow"; value = 75; } { color = "red"; value = 90; } ];
      };
    }
  ];

  aiRow = mkRow {
    id = 300;
    title = "${aiName} (inference)";
    gridPos = { x = 0; y = 8; w = fullW; h = 1; };
    collapsed = true;
    panels = map mkPanel (layout2 9 aiSpecs);
  };

  mkVmRow = i: name:
    let
      mon = guests.${name}.monitoring;
      inst = ''instance="${name}"'';
      rowY = 9 + i;  # after the fleet strip (y0 row, y1..5 tiles), host (y6), NAS (y7) and ai (y8)
      base = 1000 + 100 * i;   # clear of the fixed rows, with room for any number of guests
      specs =
        common base inst
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
      expr = ''up{job="node"}'';
      colorMode = "background";
      thresholds = { mode = "absolute"; steps = [ { color = "red"; value = null; } { color = "green"; value = 1; } ]; };
      mappings = [{ type = "value"; options = { "0" = { text = "DOWN"; }; "1" = { text = "UP"; }; }; }];
      gridPos = { x = 0; y = 1; w = 7; h = 5; };
    })
    (mkStat {
      id = 3;
      title = "Disk %";
      unit = "percent";
      # The fullest filesystem per box, so a new guest or volume needs no edit here. The allowlist drops
      # the ones reporting someone else's disk: cifs is the NAS from compute, virtiofs the host from a guest.
      expr = ''
        max by(instance) (
          (1 - node_filesystem_avail_bytes{job="node",fstype=~"ext4|zfs|xfs|btrfs"}
             / node_filesystem_size_bytes{job="node",fstype=~"ext4|zfs|xfs|btrfs"}) * 100
        )
      '';
      # Tracks DiskAlmostFull, which fires at 80.
      thresholds = {
        mode = "absolute";
        steps = [ { color = "green"; value = null; } { color = "yellow"; value = 70; } { color = "red"; value = 80; } ];
      };
      gridPos = { x = 7; y = 1; w = 10; h = 5; };
    })
    (mkStat {
      id = 4;
      title = "SMART";
      # min: one failing device drops its whole host to FAIL.
      expr = ''min by(instance) (smartctl_device_smart_status)'';
      colorMode = "background";
      thresholds = { mode = "absolute"; steps = [ { color = "red"; value = null; } { color = "green"; value = 1; } ]; };
      mappings = [{ type = "value"; options = { "0" = { text = "FAIL"; }; "1" = { text = "OK"; }; }; }];
      gridPos = { x = 17; y = 1; w = 7; h = 5; };
    })
    hostRow
    storageRow
    aiRow
  ] ++ builtins.genList (i: mkVmRow i (builtins.elemAt names i)) (builtins.length names);
}
