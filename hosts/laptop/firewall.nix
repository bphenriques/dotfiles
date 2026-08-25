{ config, ... }:
let
  inherit (config.custom.fleet.lan) hosts;
in
{
  networking.nftables.enable = true;
  networking.firewall.extraInputRules = ''
    ip saddr ${hosts.compute} tcp dport ${toString config.services.ollama.port} accept comment "ollama, via compute's NAT egress"
  '';
}
