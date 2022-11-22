{
  description = "bphenriques's Nix configuration for his machines";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-22.05";               # Default to stable for most things.
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixpkgs-unstable"; # Unstable for some packages.

    darwin.url = "github:lnl7/nix-darwin/master";
    darwin.inputs.nixpkgs.follows = "nixpkgs-unstable";             # Pin Darwin to unstable.

    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs-unstable";       # Pin Home-Manager to unstable.
  };

  outputs = inputs @ { self, nixpkgs, darwin, home-manager, ... }:
    let
      inherit (inputs.nixpkgs-unstable.lib) attrValues;

      nixpkgsConfig = {
        config = { allowUnfree = true; };                     # Well..
      };

        nixConfig = {
          settings = {
            experimental-features = [ "nix-command" "flakes" ]; # Enable nix flakes.
            auto-optimise-store   = true;                       # Ensure /nix/store does not grow eternally.
          };
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
      };
    in
    {
      darwinConfigurations = {
        work-macos = macosLib.mkMacOSHost (import ./host/work-macos {});
      };

      homeManagerConfigurations = {
        wsl = homeManagerLib.mkHomeManagerHost (import ./host/wsl {});
      };

      # Handy aliases
      work-macos     = self.darwinConfigurations.work-macos.system;
      wsl            = self.homeManagerConfigurations.wsl.activationPackage;

      # Custom modules. Either adds functionality or redefines in order to finer grain control over the output.
      homeManagerModules = {
        bphenriques-zsh           = ./modules/home-manager/zsh.nix;
        bphenriques-fzf-extra     = ./modules/home-manager/fzf-extra.nix;
        bphenriques-thefuck       = ./modules/home-manager/thefuck.nix;
        bphenriques-direnv-extra  = ./modules/home-manager/direnv-extra.nix;
        bphenriques-powerlevel10k = ./modules/home-manager/powerlevel10k.nix;
      };

      darwinModules = {
        org-protocol          = ./modules/darwin/org-protocol;
        system-screencapture  = ./modules/darwin/system/screencapture;
        system-desktop        = ./modules/darwin/system/desktop;
      };
    };
}
