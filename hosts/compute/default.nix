{ nixpkgs, nur, self, sops-nix, disko, home-manager, stylix, ... }:
let
  inherit (nixpkgs.lib.attrsets) attrValues;

  system = "x86_64-linux";

  nixModule = {
    nixpkgs = {
      overlays = attrValues self.overlays ++ [ nur.overlays.default ];
    };
  };

  nixosModules = attrValues self.nixosModules ++ [
    sops-nix.nixosModules.sops
    disko.nixosModules.disko
    home-manager.nixosModules.home-manager
    jellarr.nixosModules.default
  ];

  sharedSpecialArgs = {
    self = self // {
      lib = {
        builders = import ../../lib/builders.nix { inherit (nixpkgs) lib; pkgs = nixpkgs.legacyPackages.${system}; };
      };
      pkgs = self.packages.${system};
    };
  };

  hmModule = {
    home-manager = {
      sharedModules = attrValues self.homeManagerModules ++ [
        stylix.homeModules.stylix
      ];
      extraSpecialArgs = sharedSpecialArgs;
    };
  };
in nixpkgs.lib.nixosSystem {
  inherit system;
  specialArgs = sharedSpecialArgs;
  modules = nixosModules ++ [ nixModule hmModule ./config.nix ];
}