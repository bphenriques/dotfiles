# node-exporter for compute's Prometheus, bound to the bridge IP — compute-only, on top
# of the firewall rule in microvm.nix. HTTP metrics come from Traefik itself (see
# traefik.nix → :9117), not a log exporter.
{ ... }:
let
  fleet = import ../../shared.nix;
in
{
  services.prometheus.exporters.node = {
    enable = true;
    listenAddress = fleet.microvm.hosts.share-vm;
    port = 9100;
    openFirewall = false;
  };
}
