{ shareVm, ... }:
let
  fleet = import ../../shared.nix;
in
{
  services.prometheus.exporters.node = {
    enable = true;
    listenAddress = fleet.computeMicrovm.hosts.share-vm.ip; # bridge IP (compute-only behind firewall)
    port = 9100;
    openFirewall = false;
  };
}