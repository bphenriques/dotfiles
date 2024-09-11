{ inputs, nixConfig, nixpkgsConfig, ... }:
{
  mkNixOSHost = {
    system ? "x86_64-linux",
    hostConfig,
    specialArgs ? {}
  }:
    let
      nixpkgs = inputs.nixpkgs-unstable;
      lib = nixpkgs.lib;

      homeManagerModules = [
        inputs.sops-nix.homeManagerModules.sops
      ] ++ (lib.attrsets.attrValues inputs.self.homeManagerModules);

      commonConfig = {
        nixpkgs = nixpkgsConfig;
        nix = nixConfig;

        home-manager.useGlobalPkgs    = true;               # Use pkgs set within nixpkgs.
        home-manager.useUserPackages  = true;               # Install packages defined in home-manager.
        home-manager.sharedModules    = homeManagerModules; # Custom modules.
      };
      commonModules = [
        inputs.sops-nix.nixosModules.sops
        inputs.disko.nixosModules.disko
        inputs.home-manager.nixosModules.home-manager
      ] ++ lib.attrsets.attrValues inputs.self.nixosModules;
    in nixpkgs.lib.nixosSystem {
      inherit system specialArgs;
      modules = commonModules ++ [ commonConfig hostConfig ];
    };
}
