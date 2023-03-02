{ ... }:
{
  username = "bphenriques";
  hostNixOSModules = [
    ../../nixos
    ./nixos.nix
  ];
  hostHomeManagerModules = [
    ../../home
    ./home.nix
  ];
}
