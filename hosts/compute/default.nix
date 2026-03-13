{ nixpkgs, self, sops-nix, disko, home-manager, dotfiles-private, stylix, ... }:
let
  inherit (nixpkgs.lib.attrsets) attrValues;

  system = "x86_64-linux";

  nixModule = {
    nixpkgs = {
      overlays = attrValues self.overlays;
    };
  };

  nixosModules = attrValues self.nixosModules ++ [
    sops-nix.nixosModules.sops
    disko.nixosModules.disko
    home-manager.nixosModules.home-manager
  ];

  sharedSpecialArgs = {
    self = self // {
      pkgs = self.packages.${system} // dotfiles-private.packages.${system};
      lib.builders = self.lib.builders.${system};
      private = dotfiles-private;
      shared = import ../shared.nix;
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
