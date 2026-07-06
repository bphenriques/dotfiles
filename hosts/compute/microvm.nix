# compute as microVM host: import the host-agnostic profile and feed it compute's allocation table.
{ ... }:
{
  imports = [ ../../profiles/nixos/microvm-host.nix ];

  homelab.microvm.host = {
    enable = true;
    uplink = "bond0";
    inherit (import ./guests.nix) bridge guests;
  };
}
