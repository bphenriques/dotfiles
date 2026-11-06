# Exporters every machine runs: `job` is the exporter kind, `instance` the source. Single-box exporters scrape next to their rules (ups.nix).
{ config, lib, ... }:
let
  inherit (config.fleet.lan) hosts;
  inherit (config.services.prometheus) exporters;

  hostEntries = {
    compute = {
      address = "127.0.0.1";
      ports = {
        node = exporters.node.port;
        smartctl = exporters.smartctl.port;
      };
    };
    storage = {
      address = hosts.storage;
      ports = { node = 9100; smartctl = 9633; zfs = 9134; };
    };
    ai = {
      address = hosts.ai;
      ports = { node = 9100; smartctl = 9633; };
    };
  };

  # Guests run the same node_exporter, so they join the same job, whichever host runs them.
  guestEntries = lib.mapAttrs (_: ip: {
    address = ip;
    ports = { node = 9100; };
  }) (lib.concatMapAttrs (_: guests: guests) config.fleet.microvms);

  fleet = hostEntries // guestEntries;

  # Per-kind additions merged onto the generated job.
  extra = {
    smartctl.scrape_interval = "2m"; # shells out to smartctl(8) per drive, and the values barely move
    # node_exporter can filter neither of these itself: it emits five states per unit, only one read.
    node.metric_relabel_configs = [
      {
        source_labels = [ "__name__" "state" ];
        regex = "node_systemd_unit_state;(?:activating|active|deactivating|inactive)";
        action = "drop";
      }
      {
        source_labels = [ "__name__" ];
        regex = "node_scrape_collector_duration_seconds";
        action = "drop";
      }
    ];
  };

  kinds = lib.unique (lib.concatMap (e: lib.attrNames e.ports) (lib.attrValues fleet));

  targetsFor =
    kind:
    lib.mapAttrsToList (name: e: {
      targets = [ "${e.address}:${toString e.ports.${kind}}" ];
      labels.instance = name;
    }) (lib.filterAttrs (_: e: e.ports ? ${kind}) fleet);
in
{
  selfhost.monitoring.scopes.fleet = {
    scrapeConfigs = map (
      kind:
      {
        job_name = kind;
        static_configs = targetsFor kind;
      }
      // (extra.${kind} or { })
    ) kinds;

    rules = [{
      name = "fleet";
      rules = [{
        # Hosts only (guests have GuestDown); escalates the framework's 10m PrometheusTargetDown.
        alert = "FleetHostDown";
        expr = ''up{job="node",instance=~"${lib.concatStringsSep "|" (lib.attrNames hostEntries)}"} == 0'';
        "for" = "5m";
        labels.severity = "critical";
        annotations.summary = "{{ $labels.instance }}: unreachable";
      }];
    }];
  };
}
