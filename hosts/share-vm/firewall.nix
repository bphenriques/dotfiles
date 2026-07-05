# Ingress: SSH + metrics (node-exporter 9100, Traefik 9117) from compute over the bridge
# only — the tailnet and internet match nothing and are dropped. Egress is sealed on the
# host side (hosts/compute/microvm/default.nix).
{ config, shareVm, ... }:
let
  inherit (import ../shared.nix) computeMicrovm;
  prometheusPort = toString config.services.prometheus.exporters.node.port;
  traefikMetricsPort = toString shareVm.traefikMetricsPort;
in
{
  networking.nftables.enable = true;
  networking.firewall = {
    enable = true;
    extraInputRules = "ip saddr ${computeMicrovm.bridge.gateway} tcp dport { 22, ${prometheusPort}, ${traefikMetricsPort} } accept";
  };
}
