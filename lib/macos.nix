{ darwin, home-manager, darwinModules, homeManagerModules, nixpkgsConfig, nixConfig, ... }:
{
  mkMacOSHost = {
    system ? "aarch64-darwin",
    hostModule,
  }:
    let
      common = {
        nixpkgs = nixpkgsConfig // {
          hostPlatform = system;
        };
        nix = nixConfig;

        # Nix Darwin
        services.nix-daemon.enable   = true; # Using nix-daemon (the only supported way).

        # Home-Manager
        home-manager.useGlobalPkgs   = true; # Consistency: use pkgs set via the system level nixpkgs options.
        home-manager.useUserPackages = true; # Install packages defined in home-manager.
        home-manager.sharedModules   = homeManagerModules; # My custom modules.
      };
    in darwin.lib.darwinSystem {
      inherit system;
      modules = [common home-manager.darwinModules.home-manager hostModule] ++ darwinModules;
    };
}
