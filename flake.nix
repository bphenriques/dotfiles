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

    # Specific packages
    ## Compiling Emacs GCC takes forever, let's pin it. Currently Emacs 29.0.50
    emacs-overlay.url = "github:nix-community/emacs-overlay?rev=206e22c6ba3f8cd28649d5360e2838f3bb90aa55";
  };

  outputs = { self, nixpkgs, darwin, home-manager, ... }@inputs:
    let
      inherit (inputs.nixpkgs-unstable.lib) optionalAttrs;

      # Overlays
      unstableOverlay = final: prev: {
        unstable = inputs.nixpkgs-unstable.legacyPackages.${prev.system}; # Make available unstable channel.
      };
      #emacs-overlay = import inputs.emacs-overlay;
      x86-packages = final: prev: (optionalAttrs (prev.stdenv.system == "aarch64-darwin") {
        inherit (final.pkgs-x86)
        google-cloud-sdk
        ngrok;
      });
      apple-silicon = final: prev: optionalAttrs (prev.stdenv.system == "aarch64-darwin") {
        # Add access to x86 packages system is running Apple Silicon
        pkgs-x86 = import inputs.nixpkgs-unstable {
          system = "x86_64-darwin";
          inherit (nixpkgsConfig) config;
        };
      };

      # Nixpkgs
      nixpkgsConfig = {
        config = { allowUnfree = true; }; # :monkey-close-eyes:
        overlays = [
          unstableOverlay
          x86-packages
          #emacs-overlay
          apple-silicon
        ];
      };
      macosLib = import ./lib/macos.nix { inherit darwin home-manager; nixpkgs=nixpkgsConfig; };
      hmLib = import ./lib/home-manager.nix { inherit home-manager; nixpkgs=nixpkgsConfig; };
    in
    {
      darwinConfigurations = with macosLib; {
        personal-macos = mkMacOSHost { hostModule=./hosts/personal-macos.nix; system="x86_64-darwin"; };
        work-macos = mkMacOSHost { hostModule=./hosts/work-macos.nix; system="aarch64-darwin"; };
      };

      homeManagerConfigurations = with hmLib; {
        wsl = mkHMHost {
          username = "bphenriques";
          homeConfig = ./hosts/wsl.nix;
        };
      };

      # Handy aliases 
      work-macos     = self.darwinConfigurations.work-macos.system;
      personal-macos = self.darwinConfigurations.personal-macos.system;
      wsl            = self.homeManagerConfigurations.wsl.activationPackage;
    };
}
