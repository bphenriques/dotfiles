{ self, nixpkgs, home-manager, darwin, dotfiles-private, ... }:
let
  inherit (nixpkgs.lib.attrsets) attrValues;

  system = "aarch64-darwin";
  nixModule = {
    nixpkgs = {
      hostPlatform = system;
      overlays = attrValues self.overlays ++ [ nur.overlays.default ];
    };
  };

  darwinModules = attrValues self.darwinModules ++ [ home-manager.darwinModules.home-manager ];

  sharedSpecialArgs = {
    self = self // {
      lib = {
        builders = import ../../lib/builders.nix { inherit (nixpkgs) lib; pkgs = nixpkgs.legacyPackages.${system}; };
      };
      pkgs = self.packages.${system} // dotfiles-private.packages.${system};
    };
  };

  hmModule = {
    home-manager = {
      sharedModules = attrValues self.homeManagerModules ++ [
        stylix.homeManagerModules.stylix
      ];
      extraSpecialArgs = sharedSpecialArgs;
    };
  };

in darwin.lib.darwinSystem {
  inherit system sharedSpecialArgs;
  modules = darwinModules ++ [ nixModule hmModule ./config.nix ];
}