{ config, lib, ... }:
let
  inherit (config.fleet.lan) hosts;
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

  # Samba stays LAN-wide (laptop and phone mount it); the rest answers only to compute.
  networking.firewall.extraInputRules = ''
    ip saddr ${hosts.compute} tcp dport { ${dports exporterPorts} } accept comment "exporters, scraped by compute"
    ip saddr { ${hosts.compute}, ${hosts.ai} } tcp dport { ${dports upsdPorts} } accept comment "upsd, monitored by the secondary upsmons"
  '';
}
