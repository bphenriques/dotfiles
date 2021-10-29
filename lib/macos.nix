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
  mkMacOSHost = hostModule: darwin.lib.darwinSystem {
    # system = "aarch64-darwin";
    system = "x86_64-darwin";
    modules = [
      home-manager.darwinModules.home-manager
      ({ pkgs, ... }:
        {
          nix.package = pkgs.nixFlakes;
          nix.extraOptions = "experimental-features = nix-command flakes";
        }
      )
      {
        nixpkgs = nixpkgs;
        home-manager.useGlobalPkgs = true;      # For consistency, use global pkgs configured via the system level nixpkgs options.
        home-manager.useUserPackages = true;    # Install packages defined in home-manager.

        # Nix-Darwin
        system.stateVersion = 4;
      }
      hostModule
    ];
  };
}
