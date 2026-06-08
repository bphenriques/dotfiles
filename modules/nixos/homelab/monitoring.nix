{ config, lib, pkgs, ... }:
let
  cfg = config.custom.homelab;
  mon = cfg.monitoring;
  prometheusCfg = cfg.services.prometheus;
  alertmanagerCfg = cfg.services.alertmanager;
  yaml = pkgs.formats.yaml { };

  monitoringScopeModule = _: {
    options = {
      enable = lib.mkEnableOption "this monitoring scope" // { default = true; };

      exporters = lib.mkOption {
        type = lib.types.attrsOf lib.types.anything;
        default = { };
        description = "Prometheus exporter definitions merged into services.prometheus.exporters";
      };

      scrapeConfigs = lib.mkOption {
        type = lib.types.listOf lib.types.attrs;
        default = [ ];
        description = "Prometheus scrape configurations";
      };

      rules = lib.mkOption {
        type = lib.types.listOf lib.types.attrs;
        default = [ ];
        description = "Prometheus alert rule groups";
      };

      systemdOverrides = lib.mkOption {
        type = lib.types.attrsOf lib.types.anything;
        default = { };
        description = "Systemd service overrides for monitoring-related units";
      };
    };
  };

  # Standalone monitoring scopes (infrastructure, hardware, etc.)
  enabledScopes = lib.filter (s: s.enable) (lib.attrValues cfg.monitoring.scopes);

  # Service-level monitoring scopes (custom exporters per service)
  servicesWithMonitoring = lib.filter
    (s: s.integrations.monitoring.enable)
    (lib.attrValues cfg.services);

  serviceMonitoringScopes = map (s: {
    inherit (s.integrations.monitoring) exporters scrapeConfigs rules systemdOverrides;
  }) servicesWithMonitoring;

  # Services with healthcheck probes
  healthcheckedServices = lib.filter
    (s: s.integrations.monitoring.healthcheck)
    servicesWithMonitoring;

  hasHealthchecks = healthcheckedServices != [ ];

  # Blackbox exporter scope (auto-generated from healthchecked services)
  blackboxExporters = lib.optionalAttrs hasHealthchecks {
    blackbox = {
      enable = true;
      listenAddress = "127.0.0.1";
      port = 9116;
      configFile = yaml.generate "blackbox.yml" {
        modules = {
          http_2xx = {
            prober = "http";
            timeout = "5s";
            http = {
              valid_http_versions = [ "HTTP/1.1" "HTTP/2.0" ];
              valid_status_codes = [ ];
              follow_redirects = true;
              preferred_ip_protocol = "ip4";
            };
          };
          http_any = {
            prober = "http";
            timeout = "5s";
            http = {
              valid_http_versions = [ "HTTP/1.0" "HTTP/1.1" "HTTP/2.0" ];
              valid_status_codes = [ 200 204 301 302 303 307 308 401 403 ];
              follow_redirects = false;
              preferred_ip_protocol = "ip4";
            };
          };
        };
      };
    };
  };

  blackboxScrapeConfigs = lib.optionals hasHealthchecks (let
    byModule = builtins.groupBy (s: s.healthcheck.probeModule) healthcheckedServices;
  in lib.mapAttrsToList (moduleName: services: {
    job_name = "healthcheck-${moduleName}";
    scrape_interval = "300s";  # Reduce wakeups: healthcheck probes don't need 60s resolution
    metrics_path = "/probe";
    params.module = [ moduleName ];
    static_configs = map (s: {
      targets = [ s.healthcheck.url ];
      labels.instance = s.name;
    }) services;
    relabel_configs = [
      { source_labels = [ "__address__" ]; target_label = "__param_target"; }
      { target_label = "__address__"; replacement = "127.0.0.1:9116"; }
    ];
  }) byModule);

  blackboxRules = lib.optionals hasHealthchecks [{
    name = "services";
    rules = [
      {
        alert = "ServiceDown";
        expr = "probe_success == 0";
        "for" = "6m";
        labels.severity = "critical";
        annotations.summary = "{{ $labels.instance }} unreachable";
      }
      {
        alert = "ServiceSlowResponse";
        expr = "probe_success == 1 and probe_duration_seconds > 4";
        "for" = "5m";
        labels.severity = "warning";
        annotations.summary = "{{ $labels.instance }} slow (>4s)";
      }
    ];
  }];

  # Normalize blackbox into a scope so every collector folds over one list.
  blackboxScope = {
    exporters = blackboxExporters;
    scrapeConfigs = blackboxScrapeConfigs;
    rules = blackboxRules;
    systemdOverrides = { };
  };

  allScopes = enabledScopes ++ serviceMonitoringScopes ++ [ blackboxScope ];

  # Shallow merge for exporters (duplicates caught by the assertion below).
  allExporters = lib.foldl' (acc: s: acc // s.exporters) { } allScopes;
  allExporterNames = lib.concatMap (s: lib.attrNames s.exporters) allScopes;
  dupExporterNames = lib.filter (n: lib.count (m: m == n) allExporterNames > 1)
    (lib.unique allExporterNames);

  allScrapeConfigs = lib.concatMap (s: s.scrapeConfigs) allScopes;
  allRules = lib.concatMap (s: s.rules) allScopes;
  allSystemdOverrides = map (s: s.systemdOverrides) allScopes;

  # Port collision detection (best-effort: exporter↔exporter + exporter↔service; service↔service is in services-registry.nix)
  allPortEntries = let
    fromExporters = scopeName: exporters:
      lib.concatLists (lib.mapAttrsToList (expName: expCfg:
        lib.optional (expCfg ? listenAddress && expCfg ? port) {
          name = "${scopeName}/${expName}";
          inherit (expCfg) listenAddress port;
        }
      ) exporters);
  in
    lib.concatLists (lib.mapAttrsToList (name: scope:
      fromExporters "scope/${name}" scope.exporters
    ) (lib.filterAttrs (_: s: s.enable) cfg.monitoring.scopes))
    ++ lib.concatLists (map (s:
      fromExporters "service/${s.name}" s.integrations.monitoring.exporters
    ) servicesWithMonitoring)
    ++ lib.optionals hasHealthchecks [
      { name = "healthcheck/blackbox"; listenAddress = "127.0.0.1"; port = 9116; }
    ]
    ++ map (s: { name = "service/${s.name}"; listenAddress = s.host; inherit (s) port; })
      (lib.attrValues cfg.services);

  dupPorts = lib.filter (e:
    lib.count (q: q.listenAddress == e.listenAddress && q.port == e.port) allPortEntries > 1
  ) allPortEntries;
in
{
  options.custom.homelab.monitoring = {
    enable = lib.mkEnableOption "Prometheus monitoring (metrics, healthchecks, alert rules)";

    alertmanager.enable = lib.mkEnableOption "Alertmanager alert delivery (routes fired alerts to notify)";

    retentionTime = lib.mkOption {
      type = lib.types.str;
      default = "365d";
      description = "Prometheus metrics retention time.";
    };

    retentionSize = lib.mkOption {
      type = lib.types.str;
      default = "5GB";
      description = "Prometheus metrics retention size cap (the effective bound).";
    };

    scrapeInterval = lib.mkOption {
      type = lib.types.str;
      default = "60s";
      description = "Global Prometheus scrape and evaluation interval.";
    };

    scopes = lib.mkOption {
      type = lib.types.attrsOf (lib.types.submodule monitoringScopeModule);
      default = { };
      description = "Monitoring scopes: infrastructure, hardware, and other non-service metric sources";
    };
  };

  config = lib.mkMerge [
    (lib.mkIf mon.enable {
      assertions = [
        {
          assertion = dupExporterNames == [ ];
          message = "Duplicate Prometheus exporter names across scopes: ${toString dupExporterNames}";
        }
        {
          assertion = dupPorts == [ ];
          message = "Monitoring port collisions: ${
            lib.concatMapStringsSep ", " (e: "${e.name} (${e.listenAddress}:${toString e.port})") dupPorts
          }";
        }
      ];

      custom.homelab.services.prometheus = {
        displayName = "Prometheus";
        description = "Metrics";
        port = 9090;
        healthcheck.path = "/-/healthy";
        forwardAuth.enable = true;
        integrations.homepage = { enable = true; tab = "Admin"; };
        integrations.monitoring = {
          scrapeConfigs = [{
            job_name = "prometheus";
            scrape_interval = "300s";
            static_configs = [{ targets = [ "127.0.0.1:${toString prometheusCfg.port}" ]; }];
          }];
          rules = [{
            name = "prometheus";
            rules = [{
              alert = "PrometheusTargetDown";
              expr = "up == 0";
              "for" = "10m";
              labels.severity = "warning";
              annotations.summary = "{{ $labels.job }}/{{ $labels.instance }} down";
            }];
          }];
        };
      };

      # Writes directly to NVMe; SSD wear is negligible at 60s with a small alert set.
      services.prometheus = {
        enable = true;
        listenAddress = prometheusCfg.host;
        inherit (prometheusCfg) port;
        inherit (mon) retentionTime;
        extraFlags = [ "--storage.tsdb.retention.size=${mon.retentionSize}" "--storage.tsdb.wal-compression" ];
        globalConfig = { scrape_interval = mon.scrapeInterval; evaluation_interval = mon.scrapeInterval; };
        exporters = allExporters;
        scrapeConfigs = allScrapeConfigs;
        ruleFiles = lib.optional (allRules != [ ]) (
          yaml.generate "alerts.yml" { groups = allRules; }
        );
      };

      systemd.services = lib.foldl' lib.recursiveUpdate { } allSystemdOverrides;
    })

    (lib.mkIf mon.alertmanager.enable {
      assertions = [{
        assertion = mon.enable;
        message = "custom.homelab.monitoring.alertmanager.enable requires custom.homelab.monitoring.enable";
      }];

      custom.homelab.services.alertmanager = {
        displayName = "Alertmanager";
        description = "Alert Routing";
        port = 9093;
        healthcheck.path = "/-/healthy";
        forwardAuth.enable = true;
        integrations.homepage = { enable = true; tab = "Admin"; };
        integrations.notify = {
          enable = true;
          topic = lib.mkDefault "homelab-alert";
        };
      };

      custom.homelab.notify.topics."homelab-alert".public = lib.mkDefault false;

      services.prometheus = {
        alertmanagers = [{
          static_configs = [{ targets = [ "127.0.0.1:${toString alertmanagerCfg.port}" ]; }];
        }];

        alertmanager = {
          enable = true;
          listenAddress = alertmanagerCfg.host;
          inherit (alertmanagerCfg) port;
          configuration = {
            route = {
              receiver = "notify";
              group_by = [ "alertname" ];
              group_wait = "30s";
              group_interval = "5m";
              repeat_interval = "4h";
            };
            receivers = [{
              name = "notify";
              webhook_configs = [{
                url = "${cfg.notify.url}/${alertmanagerCfg.integrations.notify.topic}?template=alertmanager";
                send_resolved = true;
                http_config.authorization = {
                  type = "Bearer";
                  credentials_file = "/run/credentials/alertmanager.service/notify-token";
                };
              }];
            }];
          };
        };
      };

      systemd.services.alertmanager = {
        after = [ "ntfy-configure.service" ];
        wants = [ "ntfy-configure.service" ];
        serviceConfig.LoadCredential = [ "notify-token:${alertmanagerCfg.integrations.notify.tokenFile}" ];
      };
    })
  ];
}
