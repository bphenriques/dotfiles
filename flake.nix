{
  description = "Bruno Henriques's Nix configuration for his machines";

  inputs = {
    # Packages
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";

    # MacOS inputs
    darwin.url = "github:lnl7/nix-darwin/master";
    darwin.inputs.nixpkgs.follows = "nixpkgs";

    # Home inputs
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    # Custom
    neovim.url = "github:neovim/neovim/4be0e92db01a502863ac4bb26dd0fee16d833145?dir=contrib";
    neovim.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, darwin, home-manager, ... }@inputs:
    let
    overlays = with inputs; [
        (
          final: prev: {
            # Additional neo-vim package provided within the neovim repository.
            neovim-nightly = neovim.packages.${prev.stdenv.system}.neovim;
          }
        )
      ];
      nixpkgsConfig = with inputs; {
        config = { allowUnfree = true; }; # :nothing-to-see-here:
        overlays = overlays;
      };
      nixDarwinHelpers = import ./lib/nix-darwin-helpers.nix { inherit darwin home-manager; nixpkgs=nixpkgsConfig; };
    in
    {
      darwinConfigurations = with nixDarwinHelpers; {
        personal-macos = mkMacOSHost ./hosts/personal-macos.nix;
        work-macos = mkMacOSHost ./hosts/work-macos.nix;
      };
    };
}
