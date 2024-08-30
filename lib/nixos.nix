{ home-manager, nixosModules, homeManagerModules, nixpkgsConfig, nixConfig, nixpkgs, ... }:
{
  mkNixOSHost = {
    system ? "x86_64-linux",
    hostModule,
    specialArgs ? {}
  }:
    let
      common = {
        nixpkgs = nixpkgsConfig;
        nix = nixConfig;

        home-manager.useGlobalPkgs    = true;               # Use pkgs set within nixpkgs.
        home-manager.useUserPackages  = true;               # Install packages defined in home-manager.

        home-manager.sharedModules    = homeManagerModules; # My custom modules.
      };
    in nixpkgs.lib.nixosSystem {
      inherit system specialArgs;
      modules = [home-manager.nixosModules.home-manager common hostModule] ++ nixosModules;
    };
}
