{ config, lib, ... }:
let
  inherit (config.custom.fleet.lan) hosts;
  inherit (config.services.prometheus) exporters;

  dports = ports: lib.concatMapStringsSep ", " toString ports;
  exporterPorts = [
    exporters.node.port
    exporters.smartctl.port
    exporters.zfs.port
  ];
  upsdPorts = map (l: l.port) config.power.ups.upsd.listen;
in
{
  networking.nftables.enable = true;

  # Samba stays LAN-wide in ./services/samba.nix: laptop, inky and the phone all mount it. Everything
  # else answers to compute alone, and none of it authenticates beyond NUT's shared password.
  networking.firewall.extraInputRules = ''
    ip saddr ${hosts.compute} tcp dport { ${dports exporterPorts} } accept comment "exporters, scraped by compute"
    ip saddr ${hosts.compute} tcp dport { ${dports upsdPorts} } accept comment "upsd, monitored by compute's secondary upsmon"
  '';
}
