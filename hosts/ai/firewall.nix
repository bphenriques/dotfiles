{ config, ... }:
let
  inherit (config.fleet.lan) hosts;
  inherit (config.fleet.ai) endpoint;
  inherit (config.services.prometheus) exporters;
in
{
  networking.nftables.enable = true;

  # Nothing behind these ports authenticates, so reachability is the access control.
  networking.firewall.extraInputRules = ''
    ip saddr ${hosts.compute} tcp dport { ${toString exporters.node.port}, ${toString exporters.smartctl.port} } accept comment "exporters, scraped by compute"
    ip saddr { ${hosts.compute}, ${hosts.laptop} } tcp dport ${toString endpoint.port} accept comment "inference endpoint"
    ip saddr ${hosts.laptop} tcp dport 8080 accept comment "ad-hoc experiments; rootless publishing binds a host socket, so this chain does see it"
  '';
}
