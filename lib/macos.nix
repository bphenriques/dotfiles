{ darwin
, home-manager
, nixpkgs
, ...
}:

{
  # Creates a MacOS host with the shared settings plus the configuration provided.  
  # Build with:
  # $ nix build .#darwinConfigurations.<host-name>.system
  # $ ./result/sw/bin/darwin-rebuild switch --flake .#<host-name>
  mkMacOSHost = {hostModule, system ? "aarch64-darwin"}: darwin.lib.darwinSystem {
    system = system;
    modules = [
      home-manager.darwinModules.home-manager
      ({ pkgs, lib, ... }:
        {
          nix.package = pkgs.nixVersions.stable;
        }
      )
      {
        nixpkgs = nixpkgs;
        home-manager.useGlobalPkgs = true;      # For consistency, use global pkgs configured via the system level nixpkgs options.
        home-manager.useUserPackages = true;    # Install packages defined in home-manager.
        services.nix-daemon.enable = true;      # Using nix-daemon (the only supported way).

        nix.settings = {
          experimental-features = [ "nix-command" "flakes" ]; # Enable nix flakes.
          auto-optimise-store = true;                         # Ensure /nix/store does not grow eternally.
        };

        system.stateVersion = 4;                              # Nix-Darwin config version.
      }
      hostModule
    ];
  };
}
