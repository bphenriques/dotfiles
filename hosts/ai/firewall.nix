{ config, ... }:
let
  inherit (config.custom.fleet.lan) hosts;
  inherit (config.custom.fleet.ai) endpoint;
  inherit (config.services.prometheus) exporters;
in
{
  networking.nftables.enable = true;

  # The endpoint has no authentication, so reachability is the access control. Ollama runs with
  # --network=host so this chain actually sees its traffic; a published port would bypass it.
  networking.firewall.extraInputRules = ''
    ip saddr ${hosts.compute} tcp dport { ${toString exporters.node.port}, ${toString exporters.smartctl.port} } accept comment "exporters, scraped by compute"
    ip saddr { ${hosts.compute}, ${hosts.laptop} } tcp dport ${toString endpoint.port} accept comment "inference endpoint; compute covers agent-vm's NAT egress"
  '';
}
