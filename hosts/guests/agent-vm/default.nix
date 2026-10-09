_:
let
  agentVm = import ./settings.nix;
in
{
  imports = [
    ../../../profiles/nixos/guest.nix
    ./microvm.nix
    ./services
  ];

  _module.args.agentVm = agentVm;

  my.microvm.guest = {
    inherit (agentVm) stateRoot;        # SSH host key + hermes state
    ingressPorts = [ agentVm.apiPort agentVm.webuiPort ]; # hermes API and web UI, both fronted by compute's Traefik
  };

  system.stateVersion = "26.05";
}
