{ config, ... }:
let
  alfredoVm = import ./settings.nix;
in
{
  imports = [
    ../../../../../profiles/nixos/guest.nix
    ./microvm.nix
    ./shares.nix
    ./services
  ];

  _module.args.alfredoVm = alfredoVm;

  my.microvm.guest = {
    inherit (alfredoVm) stateRoot;                              # SSH host key + hermes state
    tap = "vm-alfredo";                                         # vm-<hostname> would exceed IFNAMSIZ
    ingressPorts = [ config.fleet.ai.assistantEndpoint.port ]; # hermes API, fronted by compute's Traefik
    ingressFrom = [ config.fleet.lan.hosts.compute ];          # Traefik, Open WebUI and Prometheus
  };

  system.stateVersion = "26.05";
}
