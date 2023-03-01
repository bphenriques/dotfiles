{
  description = "bphenriques's Nix configuration for his machines";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-22.11";               # Default to stable for most things.
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixpkgs-unstable"; # Unstable for some packages.

    darwin.url = "github:lnl7/nix-darwin/master";
    darwin.inputs.nixpkgs.follows = "nixpkgs-unstable";             # Pin Darwin to unstable.

    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs-unstable";       # Pin Home-Manager to unstable.
  };

  outputs = inputs @ { self, nixpkgs, darwin, home-manager, ... }:
    let
      nixpkgsConfig = {
        config = { allowUnfree = true; };                     # Well..
      };

      nixConfig = {
        settings = {
          experimental-features = [ "nix-command" "flakes" ]; # Enable nix flakes.
          auto-optimise-store   = true;                       # Ensure /nix/store does not grow eternally.
        };
      };

      nixosLib = import ./lib/nixos.nix {
        inherit home-manager nixpkgsConfig nixConfig;           # Modules and configurations.
        nixpkgs = inputs.nixpkgs-unstable;            # Requires specific stage of nixpkgs.
        nixosModules = {};                            # Nothing for now.
        homeManagerModules = self.homeManagerModules; # Custom home-manager modules.
        lib = inputs.nixpkgs-unstable.lib;            # Requires specific stage of nixpkgs.
      };

      macosLib = import ./lib/macos.nix {
        inherit darwin home-manager nixpkgsConfig nixConfig;  # Modules and configurations.
        darwinModules = self.darwinModules;                   # Custom darwin modules.
        homeManagerModules = self.homeManagerModules;         # Custom home-manager modules.
        lib = inputs.nixpkgs-unstable.lib;                    # Requires specific stage of nixpkgs.
      };

      homeManagerLib = import ./lib/home-manager.nix {
        inherit home-manager nixpkgsConfig;           # Modules and configurations.
        nixpkgs = inputs.nixpkgs-unstable;            # Requires specific stage of nixpkgs.
        homeManagerModules = self.homeManagerModules; # Custom home-manager modules.
        lib = inputs.nixpkgs-unstable.lib;            # Requires specific stage of nixpkgs.
      };
    in {
      # No alias is required: nixos-rebuild looks for the right configurating under nixosConfigurations by default.
      nixosConfigurations = with nixosLib; {
        desktop = mkRegularNixOSHost (import (./host/desktop) {});
      };

      darwinConfigurations = with macosLib; {
        work-macos = mkMacOSHost (import (./host/work-macos) {});
      };

      homeManagerConfigurations = with homeManagerLib; {
        wsl = mkHomeManagerHost (import ./host/wsl {});
      };

      # Aliases to that sets a common interface to call: nix build ".#hosts.<target>"
      hosts = {
        work-macos = self.darwinConfigurations.work-macos.system;
        wsl        = self.homeManagerConfigurations.wsl.activationPackage;
      };

      # Custom modules. Either adds new feature or redefines functionality to have finer grain control over the output.
      homeManagerModules = import ./home/modules;
      darwinModules = import ./darwin/modules;
    };
}
