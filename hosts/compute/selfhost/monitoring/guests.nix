# Guest alert rules; the scrape targets live in fleet.nix. Selects on instance, not job: fleet.nix
# picks the job names, so these only have to know what the guests are called.
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
  selfhost.monitoring.scopes = lib.mkIf cfg.enable (lib.mkMerge [
    (lib.mapAttrs mkScope cfg.guests)
    (lib.optionalAttrs (cfg.guests != { }) { guests = guestScope; })
  ]);
}
