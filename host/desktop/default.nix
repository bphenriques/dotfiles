{ ... }:
{
  username = "bphenriques";
  hostNixOSModules = [
    ./nixos.nix
  ];
  hostHomeManagerModules = [
    ../../home
    ./home.nix
  ];
}
