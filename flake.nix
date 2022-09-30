{
  description = "bphenriques's Nix configuration for his machines";

  inputs = {
    # Packages
    nixpkgs.url = "github:nixos/nixpkgs/nixos-22.05";               # Default to stable for most things.
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixpkgs-unstable"; # Unstable for some packages.

    # MacOS inputs
    darwin.url = "github:lnl7/nix-darwin/master";
    darwin.inputs.nixpkgs.follows = "nixpkgs-unstable";             # Ensure versions are consistent.

    # Home inputs
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs-unstable";       # Ensure versions are consistent.

    # Specific packages
    ## Compiling Emacs GCC takes forever, let's pin it.
    emacs-overlay.url = "github:nix-community/emacs-overlay?rev=550ce5667fee8f74aa20ad6456720ed84ebdd241";
  };

  outputs = { self, nixpkgs, darwin, home-manager, ... }@inputs:
    let
      # Overlays
      unstableOverlay = final: prev: {
        unstable = inputs.nixpkgs-unstable.legacyPackages.${prev.system}; # Make available unstable channel.
      };
      emacs-overlay = import inputs.emacs-overlay;

      # Nixpkgs
      nixpkgsConfig = {
        config = { allowUnfree = true; }; # :monkey-close-eyes:
        overlays = [
          unstableOverlay
          emacs-overlay
        ];
      };
      macosLib = import ./lib/macos.nix { inherit darwin home-manager; nixpkgs=nixpkgsConfig; };
      hmLib = import ./lib/home-manager.nix { inherit home-manager; nixpkgs=nixpkgsConfig; };
    in
    {
      darwinConfigurations = with macosLib; {
        work-macos = mkMacOSHost { hostModule = ./hosts/work-macos.nix; };
      };

      homeManagerConfigurations = with hmLib; {
        wsl = mkHMHost {
          username = "bphenriques";
          homeConfig = ./hosts/wsl.nix;
        };
      };

      # Handy aliases 
      work-macos     = self.darwinConfigurations.work-macos.system;
      wsl            = self.homeManagerConfigurations.wsl.activationPackage;
    };
}
