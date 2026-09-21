{ inputs, pkgs, ... }:
{
  imports = [
    inputs.disko.nixosModules.disko
    ./system.nix
    ./pool.nix
  ];

  environment.systemPackages = [
    inputs.disko.packages.${pkgs.stdenv.hostPlatform.system}.disko
  ];
}
