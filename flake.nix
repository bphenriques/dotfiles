{
  description = "bphenriques's machines expressed using nix";

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

    # Community flakes
    sops-nix.url = "github:Mic92/sops-nix";                       # Manage secrets using sops
    sops-nix.inputs.nixpkgs.follows = "nixpkgs-unstable";
    disko.url = "github:nix-community/disko";                     # Declaratively describe my disks layout
    disko.inputs.nixpkgs.follows = "nixpkgs-unstable";
    nur.url = "github:nix-community/nur";                         # Collection of packages. Use it for Firefox extensions
    ghostty.url = "git+ssh://git@github.com/mitchellh/ghostty";   # Terminal
  };

  outputs = inputs @ { self, nixpkgs, nixpkgs-unstable, home-manager, sops-nix, disko, nur, ... }:
    let
      inherit (mylib.hosts) mkNixOSHost mkMacOSHost;
      inherit (mylib.builders) forAllSystems;
      inherit (nixpkgs.lib.attrsets) attrValues;
      mylib = import ./lib { inherit inputs; lib = nixpkgs.lib; };

      overlays = attrValues self.overlays ++ [ nur.overlay ];
      nixosModules = attrValues self.nixosModules ++ [
        sops-nix.nixosModules.sops
        disko.nixosModules.disko
        home-manager.nixosModules.home-manager
      ];
      darwinModules = attrValues self.darwinModules ++ [ home-manager.darwinModules.home-manager ];
      hmModules     = attrValues self.homeManagerModules ++ [ sops-nix.homeManagerModules.sops ];
    in {
      apps = import ./apps { inherit nixpkgs mylib; };
      packages = import ./packages { inherit nixpkgs mylib; };
      formatter = forAllSystems (system: nixpkgs.legacyPackages.${system}.nixfmt-rfc-style);
      devShells = forAllSystems (system: {
        default = import ./shell.nix { pkgs = nixpkgs-unstable.legacyPackages.${system}; };
      });
      overlays = import ./overlays { inherit inputs; };

      # Hosts
      nixosConfigurations = {
        laptop = mkNixOSHost {
          inherit nixosModules hmModules overlays;
          hostModule = ./hosts/laptop;
        };
      };
      darwinConfigurations = {
        work-macos = mkMacOSHost {
          inherit darwinModules hmModules overlays;
          hostModule = ./hosts/work-macos;
        };
      };

      # Exposed modules
      nixosModules        = import ./modules/nixos;
      homeManagerModules  = import ./modules/home-manager;
      darwinModules       = import ./modules/darwin;
    };
}
