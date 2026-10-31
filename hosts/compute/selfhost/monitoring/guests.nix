# The alerts compute raises for every microVM guest, whichever host runs it. Scrape targets live in
# fleet.nix, and alerts select on instance, not job, because fleet.nix picks the job names.
{ config, lib, self, ... }:
let
  guests = lib.concatMapAttrs (_: guests: guests) config.fleet.microvms;

  guestScope = {
    rules = [{
      name = "guests";
      rules = [
        { alert = "GuestDown"; expr = ''up{instance=~"${lib.concatStringsSep "|" (lib.attrNames guests)}"} == 0''; for = "5m"; labels.severity = "warning"; annotations.summary = "{{ $labels.instance }} is unreachable"; }
      ];
    }];
  };

  mkScope = name: _:
    let
      stateRoot = self.nixosConfigurations.${name}.config.my.microvm.guest.stateRoot;
    in
    {
      rules = [{
        inherit name;
        rules = [
          { alert = "GuestHighCpu"; expr = ''(1 - avg by(instance) (rate(node_cpu_seconds_total{instance="${name}",mode="idle"}[5m]))) * 100 > 90''; for = "15m"; labels.severity = "warning"; annotations.summary = "${name} CPU over 90% for 15m"; }
          { alert = "GuestUnitFailed"; expr = ''node_systemd_unit_state{instance="${name}",state="failed"} == 1''; for = "15m"; labels.severity = "warning"; annotations.summary = "{{ $labels.name }} on ${name} has failed"; }
          { alert = "GuestStorageNearCap"; expr = ''node_filesystem_avail_bytes{mountpoint="${stateRoot}",fstype="ext4"} / node_filesystem_size_bytes{mountpoint="${stateRoot}",fstype="ext4"} < 0.1''; for = "15m"; labels.severity = "warning"; annotations.summary = "${stateRoot} on ${name} is over 90% full"; }
        ];
      }];
    };
in
{
  selfhost.monitoring.scopes = lib.optionalAttrs (guests != { }) (lib.mapAttrs mkScope guests // { guests = guestScope; });
}
