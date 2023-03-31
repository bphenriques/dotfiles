{ home-manager, nixosModules, homeManagerModules, nixpkgsConfig, nixConfig, nixpkgs, lib, ... }:
{
  mkNixOSHost = {
    system ? "x86_64-linux",
    hostModule,
  }:
    let
      inherit (lib) attrValues;
      common = {
        nixpkgs = nixpkgsConfig;
        nix = nixConfig;

        # Home-Manager
        home-manager.useGlobalPkgs    = true; # Consistency: use pkgs set via the system level nixpkgs options.
        home-manager.useUserPackages  = true; # Install packages defined in home-manager.
        home-manager.sharedModules    = attrValues homeManagerModules; # My custom modules.
      };
    in nixpkgs.lib.nixosSystem {
      inherit system;
      modules = [home-manager.nixosModules.home-manager common hostModule] ++ attrValues nixosModules;
    };
}
