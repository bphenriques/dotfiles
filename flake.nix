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

  outputs = { self, nixpkgs, darwin, home-manager, ... }@inputs:
    let
      nixpkgsConfig = {
        config = { allowUnfree = true; };
      };
      macosLib = import ./lib/macos.nix { inherit darwin home-manager; nixpkgs = nixpkgsConfig; };
      hmLib = import ./lib/home-manager.nix { inherit home-manager; nixpkgs = nixpkgsConfig; };
    in
    {
      darwinConfigurations = with macosLib; {
        work-macos = mkMacOSHost ./hosts/work-macos.nix;
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
