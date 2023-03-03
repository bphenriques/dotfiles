{ home-manager, nixosModules, homeManagerModules, nixpkgsConfig, nixConfig, nixpkgs, lib, ... }:
{
  mkRegularNixOSHost = {
    system ? "x86_64-linux",
    username,
    hostNixOSModules ? [],
    hostHomeManagerModules ? []
  }:
    let
      inherit (lib) attrValues;
      common = {
        nixpkgs = nixpkgsConfig;
        nix = nixConfig;

        # Home-Manager
        home-manager.useGlobalPkgs        = true; # Consistency: use pkgs set via the system level nixpkgs options.
        home-manager.useUserPackages      = true; # Install packages defined in home-manager.
        home-manager.users."${username}"  = {
          imports = attrValues homeManagerModules;
        };
      };

      host = {
        imports = hostNixOSModules;
        home-manager.users."${username}" = {
          imports = hostHomeManagerModules;
        };
      };
    in nixpkgs.lib.nixosSystem {
      inherit system;
      modules = [home-manager.nixosModules.home-manager common host] ++ attrValues nixosModules;
    };
}
