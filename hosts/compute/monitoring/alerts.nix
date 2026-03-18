{ config, lib, pkgs, ... }:
let
  cfg = config.custom.homelab;
  yaml = pkgs.formats.yaml { };
  hostname = config.networking.hostName;
  monitoredServices = lib.filter
    (s: s.integrations.monitoring.enable)
    (lib.attrValues cfg.services);

  # Each scope groups its exporter, scrape config, and alert rules.
  # Out of scope:
  # - mysql as it only serves no-critical services.
  scopes = [
    # System metrics: CPU, RAM, disk, temperature, network
    {
      enable = true;
      exporters.node = {
        enable = true;
        listenAddress = "127.0.0.1";
        port = 9101; # 9100 is taken by OpenCloud's built-in metrics
        enabledCollectors = [ "hwmon" "rapl" "systemd" "thermal_zone" ]; # rapl: power consumption via Intel RAPL (MSR)
      };
      scrapeConfigs = [{
        job_name = "node";
        static_configs = [{
          targets = [ "127.0.0.1:9101" ];
          labels.instance = hostname;
        }];
      }];
      rules = [{
        name = "system";
        rules = [
          {
            alert = "HighCPU";
            expr = ''(1 - avg by(instance) (rate(node_cpu_seconds_total{mode="idle"}[5m]))) * 100 > 90'';
            "for" = "5m";
            labels.severity = "warning";
            annotations.summary = "CPU > 90%";
          }
          {
            alert = "HighMemory";
            expr = "(1 - node_memory_MemAvailable_bytes / node_memory_MemTotal_bytes) * 100 > 85";
            "for" = "5m";
            labels.severity = "warning";
            annotations.summary = "Memory > 85%";
          }
          {
            alert = "DiskAlmostFull";
            expr = ''(1 - node_filesystem_avail_bytes{mountpoint="/",fstype!~"tmpfs|overlay|squashfs"} / node_filesystem_size_bytes{mountpoint="/",fstype!~"tmpfs|overlay|squashfs"}) * 100 > 80'';
            "for" = "5m";
            labels.severity = "critical";
            annotations.summary = "Disk > 80%";
          }
          {
            alert = "HighTemperature";
            expr = "max by(instance) (node_hwmon_temp_celsius) > 80";
            "for" = "2m";
            labels.severity = "critical";
            annotations.summary = "Temp > 80°C";
          }
          {
            alert = "NASStorageFull";
            expr = ''(1 - node_filesystem_avail_bytes{fstype="cifs"} / node_filesystem_size_bytes{fstype="cifs"}) * 100 > 85'';
            "for" = "5m";
            labels.severity = "warning";
            annotations.summary = "{{ $labels.mountpoint }} > 85%";
          }
        ];
      }];
    }

    # NVMe SMART health: scraped every 5m since attributes change slowly
    {
      enable = true;
      exporters.smartctl = {
        enable = true;
        listenAddress = "127.0.0.1";
        port = 9633;
      };
      scrapeConfigs = [{
        job_name = "smartctl";
        scrape_interval = "5m";
        static_configs = [{
          targets = [ "127.0.0.1:9633" ];
          labels.instance = hostname;
        }];
      }];
      rules = [{
        name = "disk-health";
        rules = [
          {
            alert = "SMARTDiskUnhealthy";
            expr = "smartctl_device_smart_status == 0";
            "for" = "0m";
            labels.severity = "critical";
            annotations.summary = "{{ $labels.device }}: SMART unhealthy";
          }
          {
            alert = "SMARTHighWearLevel";
            expr = "smartctl_device_percentage_used > 80";
            "for" = "0m";
            labels.severity = "warning";
            annotations.summary = "{{ $labels.device }}: {{ $value }}% wear";
          }
          {
            alert = "SMARTCriticalWarning";
            expr = "smartctl_device_critical_warning > 0";
            "for" = "0m";
            labels.severity = "critical";
            annotations.summary = "{{ $labels.device }}: SMART critical warning";
          }
        ];
      }];
    }

    # PostgreSQL: backs Miniflux + Immich (critical services)
    {
      enable = true;
      exporters.postgres = {
        enable = true;
        listenAddress = "127.0.0.1";
        port = 9187;
        runAsLocalSuperUser = true;
      };
      scrapeConfigs = [{
        job_name = "postgres";
        static_configs = [{
          targets = [ "127.0.0.1:9187" ];
          labels.instance = hostname;
        }];
      }];
      rules = [{
        name = "postgres";
        rules = [
          {
            alert = "PostgresDown";
            expr = "pg_up == 0";
            "for" = "2m";
            labels.severity = "critical";
            annotations.summary = "PostgreSQL down";
          }
          {
            alert = "PostgresHighConnections";
            expr = "sum by(instance) (pg_stat_activity_count) > 80";
            "for" = "5m";
            labels.severity = "warning";
            annotations.summary = "PostgreSQL > 80 connections";
          }
        ];
      }];
    }

    # Healthcheck probes for all monitored services
    {
      enable = true;
      exporters.blackbox = {
        enable = true;
        listenAddress = "127.0.0.1";
        port = 9116; # 9115 is taken by OpenCloud
        configFile = yaml.generate "blackbox.yml" {
          modules.http_2xx = {
            prober = "http";
            timeout = "5s";
            http = {
              valid_http_versions = [ "HTTP/1.1" "HTTP/2.0" ];
              valid_status_codes = [];
              follow_redirects = true;
              preferred_ip_protocol = "ip4";
            };
          };
        };
      };
      scrapeConfigs = [{
        job_name = "healthcheck";
        metrics_path = "/probe";
        params.module = [ "http_2xx" ];
        static_configs = map (s: {
          targets = [ s.healthcheck.url ];
          labels.instance = s.name;
        }) monitoredServices;
        relabel_configs = [
          { source_labels = [ "__address__" ]; target_label = "__param_target"; }
          { target_label = "__address__"; replacement = "127.0.0.1:9116"; }
        ];
      }];
      rules = [{
        name = "services";
        rules = [
          {
            alert = "ServiceDown";
            expr = "probe_success == 0";
            "for" = "3m";
            labels.severity = "critical";
            annotations.summary = "{{ $labels.instance }} unreachable";
          }
          {
            alert = "ServiceSlowResponse";
            expr = "probe_success == 1 and probe_duration_seconds > 10";
            "for" = "5m";
            labels.severity = "warning";
            annotations.summary = "{{ $labels.instance }} slow (>10s)";
          }
        ];
      }];
    }

    # Traefik request metrics (scrape only, no alerts)
    {
      enable = true;
      exporters = { };
      scrapeConfigs = [{
        job_name = "traefik";
        static_configs = [{
          targets = [ "127.0.0.1:${toString cfg.ingress.metricsPort}" ];
          labels.instance = hostname;
        }];
      }];
      rules = [];
    }

    # WireGuard peer metrics (scrape only, no alerts)
    {
      enable = true;
      exporters.wireguard = {
        enable = true;
        listenAddress = "127.0.0.1";
        port = 9586;
        latestHandshakeDelay = true;
      };
      scrapeConfigs = [{
        job_name = "wireguard";
        static_configs = [{
          targets = [ "127.0.0.1:9586" ];
          labels.instance = hostname;
        }];
      }];
      rules = [];
    }

    # Prometheus self-monitoring (always on)
    {
      enable = true;
      exporters = { };
      scrapeConfigs = [{
        job_name = "prometheus";
        static_configs = [{
          targets = [ "127.0.0.1:${toString cfg.services.prometheus.port}" ];
        }];
      }];
      rules = [{
        name = "prometheus";
        rules = [{
          alert = "PrometheusTargetDown";
          expr = "up == 0";
          "for" = "5m";
          labels.severity = "warning";
          annotations.summary = "{{ $labels.job }}/{{ $labels.instance }} down";
        }];
      }];
    }
  ];

  enabledScopes = lib.filter (s: s.enable) scopes;
in
{
  services.prometheus = {
    exporters = lib.foldl' lib.recursiveUpdate { } (map (s: s.exporters) enabledScopes);
    scrapeConfigs = lib.concatMap (s: s.scrapeConfigs) enabledScopes;
    ruleFiles = [
      (yaml.generate "alerts.yml" {
        groups = lib.concatMap (s: s.rules) enabledScopes;
      })
    ];
  };

  # /sys/class/powercap/intel-rapl/energy_uj is root-only; grant read access to the node exporter
  # https://github.com/prometheus/node_exporter/issues/2691
  systemd.services.prometheus-node-exporter.serviceConfig.CapabilityBoundingSet = [ "CAP_DAC_READ_SEARCH" ];
  systemd.services.prometheus-node-exporter.serviceConfig.AmbientCapabilities = [ "CAP_DAC_READ_SEARCH" ];
}
