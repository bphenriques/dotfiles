{
  description = "bphenriques's Nix configuration for his machines";

  nixConfig = {
    extra-substituters = [
      "https://nix-community.cachix.org"
      "https://nixpkgs-wayland.cachix.org"
    ];
    extra-trusted-public-keys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "nixpkgs-wayland.cachix.org-1:3lwxaILxMRkVhehr5StQprHdEo4IrE8sRho9R9HOLYA="
    ];
  };

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-24.05";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixpkgs-unstable";

    darwin.url = "github:lnl7/nix-darwin/master";
    darwin.inputs.nixpkgs.follows = "nixpkgs-unstable";

    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs-unstable";

    # Secret Manager
    sops-nix.url = "github:Mic92/sops-nix";
    sops-nix.inputs.nixpkgs.follows = "nixpkgs-unstable";

    # Automatically format disks using a declaractive specification
    disko.url = "github:nix-community/disko";
    disko.inputs.nixpkgs.follows = "nixpkgs-unstable";

    # Community flakes
    nur.url = "github:nix-community/nur";                         # Firefox extensions
    ghostty.url = "git+ssh://git@github.com/mitchellh/ghostty";   # Terminal
  };

  outputs = inputs @ { self, nixpkgs, nixpkgs-unstable, ... }:
    let
      inherit (mylib.hosts) mkNixOSHost mkMacOSHost;
      inherit (mylib.builders) forAllSystems;

      mylib = import ./lib { inherit inputs; lib = nixpkgs.lib; };
    in {
      apps = import ./apps { inherit nixpkgs mylib; };
      packages = import ./packages { inherit nixpkgs mylib; };
      formatter = forAllSystems (system: inputs.nixpkgs.legacyPackages.${system}.nixfmt-rfc-style);
      devShells = forAllSystems (system: {
        default = import ./shell.nix { pkgs = nixpkgs-unstable.legacyPackages.${system}; };
      });
      overlays = import ./overlays { inherit inputs; };

      # Hosts
      nixosConfigurations = {
        laptop = mkNixOSHost {
          hostConfig = ./hosts/laptop;
        };
      };
      darwinConfigurations = {
        work-macos = mkMacOSHost {
          hostConfig = ./hosts/work-macos;
        };
      };

      # Custom modules
      nixosModules        = import ./modules/nixos;
      homeManagerModules  = import ./modules/home-manager;
      darwinModules       = import ./modules/darwin;
    };
}
