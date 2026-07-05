{ config, shareVm, ... }:
let
  fleet = import ../../shared.nix;
in
{
  services.prometheus.exporters.node = {
    enable = true;
    listenAddress = fleet.computeMicrovm.hosts.${config.networking.hostName}.ip; # bridge IP (compute-only behind firewall)
    port = shareVm.prometheusPort;
    openFirewall = false;
  };
}