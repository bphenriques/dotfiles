{ darwin, home-manager, darwinModules, homeManagerModules, nixpkgsConfig, nixConfig, lib, ... }:
{
  mkMacOSHost = {
    system ? "aarch64-darwin",
    username,
    hostDarwinModules ? [],
    hostHomeManagerModules ? []
  }:
    let
      inherit (lib) attrValues;
      common = {
        nixpkgs = nixpkgsConfig;
        nix = nixConfig;

        # Nix Darwin
        services.nix-daemon.enable      = true;                   # Using nix-daemon (the only supported way).
        users.users."${username}".home  = "/Users/${username}";   # Set user's home.

        # Home-Manager
        home-manager.useGlobalPkgs    = true; # Consistency: use pkgs set via the system level nixpkgs options.
        home-manager.useUserPackages  = true; # Install packages defined in home-manager.
        home-manager.sharedModules    = attrValues homeManagerModules; # My custom modules.

        system.stateVersion = 4;                  # Nix-Darwin config version.
      };

      host = {
        imports = hostDarwinModules;
        home-manager.users."${username}" = {
          imports = hostHomeManagerModules;
        };
      };
    in darwin.lib.darwinSystem {
      inherit system;
      modules = [home-manager.darwinModules.home-manager common host] ++ attrValues darwinModules;
      inputs = { inherit username; };
    };
}
