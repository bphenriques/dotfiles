{ inputs, nixConfig, nixpkgsConfig, ... }:
{
  mkMacOSHost = {
    system ? "aarch64-darwin",
    hostConfig,
  }:
    let
      nixpkgs = inputs.nixpkgs-unstable;
      lib = nixpkgs.lib;

      homeManagerModules = [
        inputs.sops-nix.homeManagerModules.sops
      ] ++ (lib.attrsets.attrValues inputs.self.homeManagerModules);

      darwinModules = [
        inputs.home-manager.darwinModules.home-manager
      ] ++ (lib.attrsets.attrValues inputs.self.darwinModules);

      commonConfig = {
        nixpkgs = nixpkgsConfig // {
          hostPlatform = system;
        };
        nix = nixConfig;

        # Nix Darwin
        services.nix-daemon.enable   = true;                # Using nix-daemon (the only supported way).

        # Home-Manager
        home-manager.useGlobalPkgs    = true;               # Use pkgs set within nixpkgs.
        home-manager.useUserPackages  = true;               # Install packages defined in home-manager.
        home-manager.sharedModules    = homeManagerModules; # Custom modules.
      };
    in inputs.darwin.lib.darwinSystem {
      inherit system;
      modules = darwinModules ++ [commonConfig hostConfig];
    };
}