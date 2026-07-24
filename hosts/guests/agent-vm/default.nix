{ fleet, agentVm, ... }:
let
  adminUser = "bphenriques";
in
{
  imports = [
    ./settings.nix
    ../../../profiles/nixos/microvm-guest.nix
    ./microvm.nix
    ./services
  ];

  homelab.microvm.guest = {
    enable = true;
    inherit (agentVm) stateRoot;        # SSH host key + hermes state
    ingressPorts = [ agentVm.apiPort ]; # hermes API, reached by NextChat over the bridge
  };

  time.timeZone = "Europe/Lisbon";
  i18n.defaultLocale = "en_US.UTF-8";

  users.users.${adminUser} = {
    isNormalUser = true;
    extraGroups = [ "wheel" ];
    openssh.authorizedKeys.keys = fleet.ssh.authorizedKeys;
  };
  security.sudo.wheelNeedsPassword = false;

  system.stateVersion = "26.05";
}
