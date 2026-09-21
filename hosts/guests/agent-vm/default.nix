{ fleet, ... }:
let
  agentVm = import ./settings.nix;
  adminUser = "bphenriques";
in
{
  imports = [
    ../../../profiles/nixos/base.nix
    ./microvm.nix
    ./services
  ];

  _module.args.agentVm = agentVm;

  custom.microvm.guest = {
    inherit (agentVm) stateRoot;        # SSH host key + hermes state
    ingressPorts = [ agentVm.apiPort ]; # hermes API, reached by the chat UI over the bridge
  };

  users.users.${adminUser} = {
    isNormalUser = true;
    extraGroups = [ "wheel" ];
    openssh.authorizedKeys.keys = fleet.ssh.authorizedKeys;
  };
  security.sudo.wheelNeedsPassword = false;

  system.stateVersion = "26.05";
}
