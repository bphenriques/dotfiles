{ nixpkgs, nur, self, sops-nix, disko, home-manager, dotfiles-private, stylix, microvm, ... }:
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
    microvm.nixosModules.host # TODO: Remove once we test everything in here and turn on compute host
  ];

  sharedSpecialArgs = {
    self = self // {
      lib.builders = self.lib.builders.${system};
      pkgs = self.packages.${system} // dotfiles-private.packages.${system};
      settings = dotfiles-private.settings // {
        headless = true;
      };
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