{ darwin, home-manager, darwinModules, homeManagerModules, nixpkgsConfig, nixConfig, lib, ... }:
{
  mkMacOSHost = {
    system ? "aarch64-darwin",
    hostModule,
  }:
    let
      inherit (lib) attrValues;
      common = {
        nixpkgs = nixpkgsConfig;
        nix = nixConfig;

        # Nix Darwin
        services.nix-daemon.enable   = true; # Using nix-daemon (the only supported way).

        # Home-Manager
        home-manager.useGlobalPkgs   = true; # Consistency: use pkgs set via the system level nixpkgs options.
        home-manager.useUserPackages = true; # Install packages defined in home-manager.
        home-manager.sharedModules   = attrValues homeManagerModules; # My custom modules.
      };
    in darwin.lib.darwinSystem {
      inherit system;
      modules = [common home-manager.darwinModules.home-manager hostModule] ++ attrValues darwinModules;
    };
}
