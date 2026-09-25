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
    ingressPorts = [ agentVm.apiPort ]; # hermes API, reached by the chat UI over the bridge
  };

  system.stateVersion = "26.05";
}
