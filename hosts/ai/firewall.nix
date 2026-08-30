{ config, ... }:
let
  inherit (config.custom.fleet.lan) hosts;
  inherit (config.custom.fleet.ai) endpoint;
in
{
  networking.nftables.enable = true;

  # The endpoint has no authentication, so reachability is the access control.
  networking.firewall.extraInputRules = ''
    ip saddr ${hosts.compute} tcp dport ${toString config.services.prometheus.exporters.node.port} accept comment "node-exporter, scraped by compute"
    ip saddr { ${hosts.compute}, ${hosts.laptop} } tcp dport ${toString endpoint.port} accept comment "inference endpoint; compute covers agent-vm's NAT egress"
  '';
}
