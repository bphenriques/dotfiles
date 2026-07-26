# Single dashboard for the whole box: an always-visible fleet strip (host + guests), then one
# collapsed detail row per entity — host first (richest), then each microVM guest. Guest rows are
# generated from config.homelab.microvm.host.guests (the table that also drives scrape/alerts), so
# a new guest needs no edit here. Host and guest CPU/IO share one time axis for easy correlation:
# the guests run on the host's own cores.
hostName: guests:
let
  inherit (import ./lib.nix) mkPanel mkStat mkRow layout2 h w fullW;

  hostInst = ''instance="${hostName}"'';
  names = builtins.attrNames guests;
  guestInst = builtins.concatStringsSep "|" names;  # RE2 is fully anchored: matches node jobs, not *-traefik
  allInst = "${hostName}|${guestInst}";
  nodeJobs = "node|${guestInst}";  # host node-exporter job is "node"; each guest's job is its name
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
    gridPos = { x = 0; y = 6; w = fullW; h = 1; };
    collapsed = true;
    panels = map mkPanel (layout2 7 hostSpecs);
  };

  mkVmRow = i: name:
    let
      mon = guests.${name}.monitoring;
      inst = ''instance="${name}"'';
      rowY = 7 + i;  # after the fleet strip (y0..6) and the host row (y6)
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
    hostRow
  ] ++ builtins.genList (i: mkVmRow i (builtins.elemAt names i)) (builtins.length names);
}
