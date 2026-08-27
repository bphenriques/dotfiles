{ inputs, ... }:
{
  imports = [
    inputs.disko.nixosModules.disko
    ./os.nix
    ./pool.nix
  ];
}
