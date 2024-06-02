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

        # Home-Manager - It follows unstable by default.
        home-manager.useGlobalPkgs    = true; # Consistency: use pkgs set via the system level nixpkgs options.
        home-manager.useUserPackages  = true; # Install packages defined in home-manager.
        home-manager.sharedModules    = homeManagerModules; # My custom modules.
      };
    in nixpkgs.lib.nixosSystem {
      inherit system specialArgs;
      modules = [home-manager.nixosModules.home-manager common hostModule] ++ nixosModules;
    };
}
