# The per-guest fields compute watches on, plus the alerts. Declared here, not in the microvm module:
# that module owns the guests' networking, not what this host charts. Scrape targets live in fleet.nix,
# and alerts select on instance, not job, because fleet.nix picks the job names.
{ config, lib, ... }:
let
  cfg = config.custom.microvm.host;

  guestScope = {
    rules = [{
      name = "guests";
      rules = [
        { alert = "GuestDown"; expr = ''up{instance=~"${lib.concatStringsSep "|" (lib.attrNames cfg.guests)}"} == 0''; for = "5m"; labels.severity = "warning"; annotations.summary = "{{ $labels.instance }} is unreachable"; }
      ];
    }];
  };

  mkScope = name: g: {
    rules = [{
      inherit name;
      rules = [
        { alert = "GuestHighCpu"; expr = ''(1 - avg by(instance) (rate(node_cpu_seconds_total{instance="${name}",mode="idle"}[5m]))) * 100 > 90''; for = "15m"; labels.severity = "warning"; annotations.summary = "${name} CPU over 90% for 15m"; }
        { alert = "GuestUnitFailed"; expr = ''node_systemd_unit_state{instance="${name}",state="failed"} == 1''; for = "15m"; labels.severity = "warning"; annotations.summary = "{{ $labels.name }} on ${name} has failed"; }
      ] ++ lib.optional (g.monitoring.storageMount != null)
        { alert = "GuestStorageNearCap"; expr = ''node_filesystem_avail_bytes{mountpoint="${g.monitoring.storageMount}",fstype="ext4"} / node_filesystem_size_bytes{mountpoint="${g.monitoring.storageMount}",fstype="ext4"} < 0.1''; for = "15m"; labels.severity = "warning"; annotations.summary = "${g.monitoring.storageMount} on ${name} is over 90% full"; };
    }];
  };
in
{
  options.custom.microvm.host.guests = lib.mkOption {
    type = lib.types.attrsOf (lib.types.submodule {
      options.monitoring = {
        traefikMetrics = lib.mkOption { type = lib.types.bool; default = false; };
        storageMount = lib.mkOption { type = lib.types.nullOr lib.types.str; default = null; };
      };
    });
  };

  config.selfhost.monitoring.scopes = lib.mkIf cfg.enable (lib.mapAttrs mkScope cfg.guests // { guests = guestScope; });
}
