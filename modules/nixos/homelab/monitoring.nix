{ config, lib, pkgs, ... }:
let
  cfg = config.custom.homelab;
  yaml = pkgs.formats.yaml { };

  monitoringScopeModule = { ... }: {
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
              valid_status_codes = [];
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
    byModule = lib.groupBy (s: s.healthcheck.probeModule) healthcheckedServices;
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

  # Collect all exporters (shallow merge — duplicates caught by assertion)
  allExporters = lib.foldl' (acc: x: acc // x) { }
    (map (s: s.exporters) enabledScopes
     ++ map (s: s.exporters) serviceMonitoringScopes
     ++ [ blackboxExporters ]);

  allExporterNames = lib.concatMap lib.attrNames
    (map (s: s.exporters) enabledScopes
     ++ map (s: s.exporters) serviceMonitoringScopes
     ++ [ blackboxExporters ]);

  dupExporterNames = lib.filter (n: lib.count (m: m == n) allExporterNames > 1)
    (lib.unique allExporterNames);

  allScrapeConfigs = lib.concatMap (s: s.scrapeConfigs) enabledScopes
    ++ lib.concatMap (s: s.scrapeConfigs) serviceMonitoringScopes
    ++ blackboxScrapeConfigs;

  allRules = lib.concatMap (s: s.rules) enabledScopes
    ++ lib.concatMap (s: s.rules) serviceMonitoringScopes
    ++ blackboxRules;

  allSystemdOverrides = map (s: s.systemdOverrides) enabledScopes
    ++ map (s: s.systemdOverrides) serviceMonitoringScopes;

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
  options.custom.homelab.monitoring.scopes = lib.mkOption {
    type = lib.types.attrsOf (lib.types.submodule monitoringScopeModule);
    default = { };
    description = "Monitoring scopes: infrastructure, hardware, and other non-service metric sources";
  };

  config = lib.mkIf (cfg.enable && config.services.prometheus.enable) {
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

    services.prometheus = {
      exporters = allExporters;
      scrapeConfigs = allScrapeConfigs;
      ruleFiles = lib.optional (allRules != [ ]) (
        yaml.generate "alerts.yml" { groups = allRules; }
      );
    };

    systemd.services = lib.foldl' lib.recursiveUpdate { } allSystemdOverrides;
  };
}
