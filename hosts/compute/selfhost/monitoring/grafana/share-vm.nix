let
  inherit (import ./lib.nix) mkPanel mkRow h w fullW;
  inst = ''instance="share-vm"'';
in
{
  uid = "share-vm";
  title = "Share VM";
  tags = [ "share-vm" "node" ];
  timezone = "browser";
  schemaVersion = 39;
  refresh = "1m";
  time = { from = "now-6h"; to = "now"; };
  panels = [
    (mkRow { id = 1; title = "Resources"; gridPos = { x = 0; y = 0; w = fullW; h = 1; }; })
    (mkPanel {
      id = 2;
      title = "CPU Usage";
      expr = ''(1 - avg by(instance) (rate(node_cpu_seconds_total{${inst},mode="idle"}[5m]))) * 100'';
      legend = "CPU %";
      unit = "percent";
      gridPos = { x = 0; y = 1; inherit w h; };
    })
    (mkPanel {
      id = 3;
      title = "Memory Usage";
      expr = [
        { expr = ''node_memory_MemTotal_bytes{${inst}} - node_memory_MemAvailable_bytes{${inst}}''; legend = "Used"; }
        { expr = ''node_memory_MemAvailable_bytes{${inst}}''; legend = "Available"; }
      ];
      unit = "bytes";
      gridPos = { x = w; y = 1; inherit w h; };
    })

    (mkRow { id = 4; title = "Storage & HTTP"; gridPos = { x = 0; y = 9; w = fullW; h = 1; }; })
    (mkPanel {
      id = 5;
      title = "Share Storage (/srv/share, capped)";
      expr = [
        { expr = ''node_filesystem_size_bytes{mountpoint="/srv/share",fstype="ext4"}''; legend = "Total"; }
        { expr = ''node_filesystem_size_bytes{mountpoint="/srv/share",fstype="ext4"} - node_filesystem_avail_bytes{mountpoint="/srv/share",fstype="ext4"}''; legend = "Used"; }
      ];
      unit = "bytes";
      gridPos = { x = 0; y = 10; inherit w h; };
    })
    (mkPanel {
      id = 6;
      title = "HTTP responses by status (watch 401)";
      expr = ''sum by (code) (rate(traefik_service_requests_total{${inst}}[5m]))'';
      legend = "{{code}}";
      unit = "reqps";
      gridPos = { x = w; y = 10; inherit w h; };
    })
  ];
}
