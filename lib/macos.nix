{ darwin, home-manager, nixConfig, darwinModules, homeManagerModules, nixpkgsConfig, ... }:
{
  mkMacOSHost = hostModule:
    let
      common = {
        nixpkgs = nixpkgsConfig // {
          hostPlatform = "aarch64-darwin";
        };
        nix = nixConfig;

        # Nix Darwin
        services.nix-daemon.enable   = true;                # Using nix-daemon (the only supported way).

        # Home-Manager
        home-manager.useGlobalPkgs    = true;               # Use pkgs set within nixpkgs.
        home-manager.useUserPackages  = true;               # Install packages defined in home-manager.
        home-manager.sharedModules    = homeManagerModules; # My custom modules.
      };
    in darwin.lib.darwinSystem {
      inherit system;
      modules = [common home-manager.darwinModules.home-manager hostModule] ++ darwinModules;
    };
}
