_:
let
  alfredVm = import ./settings.nix;
in
{
  imports = [
    ../../../profiles/nixos/guest.nix
    ./microvm.nix
    ./services
  ];

  _module.args.alfredVm = alfredVm;

  my.microvm.guest = {
    inherit (alfredVm) stateRoot;        # SSH host key + hermes state
    tap = "vm-alfred";                   # vm-<hostname> would exceed IFNAMSIZ
    ingressPorts = [ alfredVm.apiPort ]; # hermes API, fronted by compute's Traefik
  };

  system.stateVersion = "26.05";
}
