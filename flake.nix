{
  description = "Bruno Henriques's Nix configuration for his machines";

  inputs = {
    # Packages
    nixpkgs.url = "github:nixos/nixpkgs/nixos-21.05";               # Default to stable for most things.
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixpkgs-unstable"; # Unstable for some packages.

    # MacOS inputs
    darwin.url = "github:lnl7/nix-darwin/master";
    darwin.inputs.nixpkgs.follows = "nixpkgs";                      # Ensure versions are consistent.

    # Home inputs
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";                # Ensure versions are consistent.
  };

  outputs = { self, nixpkgs, darwin, home-manager, ... }@inputs:
    let
      nixpkgsConfig = {
        config = { allowUnfree = true; }; # :monkey-close-eyes:
        overlays = [
          (
            final: prev: {
              unstable = inputs.nixpkgs-unstable.legacyPackages.${prev.system}; # Make available unstable channel.
            }
          )
        ];
      };
      nixDarwinHelpers = import ./lib/nix-darwin-helpers.nix { inherit darwin home-manager; nixpkgs=nixpkgsConfig; };
    in
    {
      darwinConfigurations = with nixDarwinHelpers; {
        personal-macos = mkMacOSHost ./hosts/personal-macos.nix;
        work-macos = mkMacOSHost ./hosts/work-macos.nix;
      };

      homeManagerConfigurations = {
        ubuntu-vm = home-manager.lib.homeManagerConfiguration {
            system = "x86_64-linux";
            homeDirectory = "/home/bphenriques";
            username = "bphenriques";
            stateVersion = "21.05";
            configuration = { pkgs, ... }: {
                imports = [ ./home/shared-home.nix ];
                nixpkgs = nixpkgsConfig;
            };
        };
      };

      # Handy aliases 
      work-macos     = self.darwinConfigurations.work-macos.system;
      personal-macos = self.darwinConfigurations.personal-macos.system;
      ubuntu-vm      = self.homeManagerConfigurations.ubuntu-vm.activationPackage;
    };
}
