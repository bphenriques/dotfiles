# Each microVM host's allocation table, by host. Read by flake.nix to build the guests and by shared.nix for their addresses.
{
  compute = import ./compute/microvm/guests.nix;
}
