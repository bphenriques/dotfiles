{
  description = "Bruno Henriques's Nix configuration for his machines";

  inputs = {
    # Packages
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";

    # MacOS inputs
    darwin.url = "github:lnl7/nix-darwin/master";
    darwin.inputs.nixpkgs.follows = "nixpkgs";
    darwin-malob-nixpkgs.url = "github:malob/nixpkgs"; # Remove once https://github.com/LnL7/nix-darwin/pull/262 is merged.

    # Home inputs
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, darwin, darwin-malob-nixpkgs, home-manager, ... }@inputs:
    let
      nixDarwinHelpers = import ./lib/nix-darwin-helpers.nix { 
        inherit darwin home-manager; 
        darwin-additional-modules = [ darwin-malob-nixpkgs.darwinModules.homebrew ]; 
      };
    in {
      darwinConfigurations = with nixDarwinHelpers; {
        personal-macos = mkMacOSHost ./hosts/personal-macos.nix;
        work-macos = mkMacOSHost ./hosts/work-macos.nix;
      };
    };
}
