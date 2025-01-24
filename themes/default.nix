{ lib }:
{
  lib = import ./lib.nix { inherit lib; };

  doom-one = import ./doom-one.nix;
}
