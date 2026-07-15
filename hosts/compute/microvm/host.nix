_:
let
  guests = import ./guests.nix;
in
{
  imports = [ ../../../profiles/nixos/capabilities/microvm-host.nix ];

  homelab.microvm.host = {
    enable = true;
    uplink = "bond0";
    inherit (guests) bridge guests;
  };
}
