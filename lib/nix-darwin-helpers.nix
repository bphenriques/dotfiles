{ darwin
, home-manager
, nixpkgs
, darwin-additional-modules ? [ ]
, ...
}:

{
  # Creates a MacOS host with the shared settings plus the configuration provided.  
  # Build with:
  # $ nix build .#darwinConfigurations.<host-name>.system
  # $ ./result/sw/bin/darwin-rebuild switch --flake .#<host-name>
  mkMacOSHost = hostModule: darwin.lib.darwinSystem {
    modules = [
      home-manager.darwinModules.home-manager
      ./enable-flakes.nix                                     # Can't be inline as the pkgs here does not include the nixFlakes attribute (unclear why).
      {
        nixpkgs = nixpkgs;
        home-manager.useGlobalPkgs = true;                    # For consistency, use global pkgs configured via the system level nixpkgs options.        
        home-manager.useUserPackages = true;                  # Install packages defined in home-manager.
      }
      hostModule
    ] ++ darwin-additional-modules;
  };
}
