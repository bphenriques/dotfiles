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
  mkMacOSHost = {hostModule, system}: darwin.lib.darwinSystem {
    system = system;
    modules = [
      home-manager.darwinModules.home-manager
      ({ pkgs, lib, ... }:
        {
          nix.package = pkgs.nixFlakes;
          nix.extraOptions = ''
            auto-optimise-store = true
            experimental-features = nix-command flakes
          '' + lib.optionalString (pkgs.system == "aarch64-darwin") ''
            extra-platforms = x86_64-darwin aarch64-darwin
          '';
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
