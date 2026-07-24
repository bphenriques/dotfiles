_:
let
  guests = import ./guests.nix;
in
{
  imports = [
    ../../../profiles/nixos/capabilities/microvm-host.nix
    ./agent-vm-vault.nix
    ./agent-vm-secret.nix
  ];

  homelab.microvm.host = {
    enable = true;
    uplink = "bond0";
    inherit (guests) bridge guests;
  };
}
